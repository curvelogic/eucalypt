//! LSP lifecycle tests driving `eu lsp` over stdio.
//!
//! The in-process harness in `lsp_test.rs` calls handlers directly, so it
//! cannot see the transport, the event loop or process exit.  These tests
//! spawn the real binary and speak JSON-RPC to it, as an editor does.

use std::io::{BufReader, Write};
use std::process::{Child, ChildStdin, Command, ExitStatus, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

use lsp_server::{Message, Notification, Request, RequestId, Response};
use serde_json::{json, Value};

/// How long to wait for the server to exit once it should.
const EXIT_TIMEOUT: Duration = Duration::from_secs(15);

/// How long to wait for a background type check (debug build, prelude
/// compiled from source).
const PIPELINE_TIMEOUT: Duration = Duration::from_secs(120);

/// How long to wait for an ordinary response.
const RESPONSE_TIMEOUT: Duration = Duration::from_secs(30);

/// A document with a type warning: a string where a number is declared.
const TYPE_WARNING_SOURCE: &str = "` { type: \"number\" }\nx: \"a\"\n";

const INLAY_REFRESH: &str = "workspace/inlayHint/refresh";
const SEMANTIC_REFRESH: &str = "workspace/semanticTokens/refresh";

/// A client connected to a spawned `eu lsp` process.
struct StdioClient {
    child: Child,
    stdin: Option<ChildStdin>,
    incoming: mpsc::Receiver<Message>,
}

impl StdioClient {
    /// Spawn `eu lsp` and complete the initialise handshake, advertising
    /// `capabilities` as the client capabilities.
    fn start(capabilities: Value) -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_eu"))
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("failed to spawn eu lsp");
        let stdin = child.stdin.take();
        let stdout = child.stdout.take().expect("child stdout");
        let (tx, incoming) = mpsc::channel();
        std::thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            while let Ok(Some(msg)) = Message::read(&mut reader) {
                if tx.send(msg).is_err() {
                    break;
                }
            }
        });
        let mut client = Self {
            child,
            stdin,
            incoming,
        };
        client.request(
            0,
            "initialize",
            json!({ "processId": null, "rootUri": null, "capabilities": capabilities }),
        );
        let resp = client.response_to(0);
        assert!(resp.error.is_none(), "initialize failed: {:?}", resp.error);
        client.notify("initialized", json!({}));
        client
    }

    fn send(&mut self, msg: Message) {
        let stdin = self.stdin.as_mut().expect("stdin already closed");
        msg.write(stdin).expect("failed to write to server");
        stdin.flush().expect("failed to flush to server");
    }

    fn request(&mut self, id: i32, method: &str, params: Value) {
        self.send(Message::Request(Request::new(
            RequestId::from(id),
            method.to_string(),
            params,
        )));
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(Message::Notification(Notification::new(
            method.to_string(),
            params,
        )));
    }

    fn open(&mut self, uri: &str, text: &str) {
        self.notify(
            "textDocument/didOpen",
            json!({ "textDocument": {
                "uri": uri, "languageId": "eucalypt", "version": 1, "text": text
            }}),
        );
    }

    /// Next message from the server, or `None` on timeout.
    fn recv(&self, timeout: Duration) -> Option<Message> {
        self.incoming.recv_timeout(timeout).ok()
    }

    /// Collect messages until `stop` matches one (inclusive), panicking
    /// on timeout.
    fn collect_until(&self, timeout: Duration, stop: impl Fn(&Message) -> bool) -> Vec<Message> {
        let deadline = Instant::now() + timeout;
        let mut seen = vec![];
        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            let msg = self
                .recv(remaining)
                .unwrap_or_else(|| panic!("timed out; messages so far: {seen:#?}"));
            let done = stop(&msg);
            seen.push(msg);
            if done {
                return seen;
            }
        }
    }

    /// Wait for the response to request `id`.
    fn response_to(&self, id: i32) -> Response {
        let id = RequestId::from(id);
        let msgs = self.collect_until(
            RESPONSE_TIMEOUT,
            |m| matches!(m, Message::Response(r) if r.id == id),
        );
        match msgs.into_iter().last() {
            Some(Message::Response(r)) => r,
            other => unreachable!("collect_until returned {other:?}"),
        }
    }

    /// Wait (reading nothing further from the client) for the type-check
    /// diagnostic of `uri`, returning every message seen up to it.
    fn await_type_diagnostics(&self, uri: &str) -> Vec<Message> {
        self.collect_until(PIPELINE_TIMEOUT, |m| is_type_diagnostic_for(m, uri))
    }

    /// Wait for the process to exit, returning its status, or `None` if
    /// it is still running after `timeout`.
    fn wait_exit(&mut self, timeout: Duration) -> Option<ExitStatus> {
        let deadline = Instant::now() + timeout;
        while Instant::now() < deadline {
            if let Some(status) = self.child.try_wait().expect("try_wait") {
                return Some(status);
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        None
    }
}

impl Drop for StdioClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// True if `msg` publishes a diagnostic from the type checker for `uri`.
fn is_type_diagnostic_for(msg: &Message, uri: &str) -> bool {
    let Message::Notification(n) = msg else {
        return false;
    };
    n.method == "textDocument/publishDiagnostics"
        && n.params["uri"] == uri
        && n.params["diagnostics"]
            .as_array()
            .is_some_and(|ds| ds.iter().any(|d| d["source"] == "eucalypt-types"))
}

/// Methods of the server-to-client requests among `msgs`.
fn server_request_methods(msgs: &[Message]) -> Vec<String> {
    msgs.iter()
        .filter_map(|m| match m {
            Message::Request(r) => Some(r.method.clone()),
            _ => None,
        })
        .collect()
}

/// A file URI for a document that need not exist on disk.
fn doc_uri(name: &str) -> String {
    let path = std::env::temp_dir().join("eu-lsp-stdio-test").join(name);
    lsp_types::Url::from_file_path(path)
        .expect("absolute temp path")
        .to_string()
}

fn hover_params(uri: &str) -> Value {
    json!({ "textDocument": { "uri": uri }, "position": { "line": 1, "character": 0 } })
}

// ── Exit ─────────────────────────────────────────────────────────────────────

/// Regression (eu-oaz72.9): after shutdown + exit the server hung joining
/// its I/O threads, because the main loop only borrowed the connection and
/// the stdout writer thread never ended.  Stdin stays open throughout.
#[test]
fn exits_zero_after_shutdown_and_exit() {
    let mut client = StdioClient::start(json!({}));
    client.request(1, "shutdown", Value::Null);
    let resp = client.response_to(1);
    assert!(resp.error.is_none(), "shutdown failed: {:?}", resp.error);
    client.notify("exit", Value::Null);
    let status = client
        .wait_exit(EXIT_TIMEOUT)
        .expect("server did not exit after shutdown + exit");
    assert_eq!(status.code(), Some(0));
}

/// Regression (eu-oaz72.9): closing stdin did not stop the server.
#[test]
fn exits_zero_when_stdin_closes() {
    let mut client = StdioClient::start(json!({}));
    client.stdin = None;
    let status = client
        .wait_exit(EXIT_TIMEOUT)
        .expect("server did not exit after stdin closed");
    assert_eq!(status.code(), Some(0));
}

/// The LSP specification: `exit` without a prior `shutdown` exits 1.
#[test]
fn exit_without_shutdown_exits_one() {
    let mut client = StdioClient::start(json!({}));
    client.notify("exit", Value::Null);
    let status = client
        .wait_exit(EXIT_TIMEOUT)
        .expect("server did not exit on exit notification");
    assert_eq!(status.code(), Some(1));
}

/// Requests after `shutdown` are answered with InvalidRequest, and the
/// server still exits cleanly on `exit`.
#[test]
fn requests_after_shutdown_are_rejected() {
    let mut client = StdioClient::start(json!({}));
    client.request(1, "shutdown", Value::Null);
    client.response_to(1);
    client.request(2, "textDocument/hover", hover_params(&doc_uri("after.eu")));
    let resp = client.response_to(2);
    let err = resp.error.expect("request after shutdown was not rejected");
    assert_eq!(err.code, lsp_server::ErrorCode::InvalidRequest as i32);
    client.notify("exit", Value::Null);
    let status = client.wait_exit(EXIT_TIMEOUT).expect("server did not exit");
    assert_eq!(status.code(), Some(0));
}

// ── Diagnostics ──────────────────────────────────────────────────────────────

/// Regression (eu-oaz72.9): background type-check results were only
/// applied when another client message arrived, so an idle client never
/// saw type diagnostics.  The client sends nothing after didOpen.
#[test]
fn type_diagnostics_published_without_client_traffic() {
    let mut client = StdioClient::start(json!({}));
    let uri = doc_uri("idle.eu");
    client.open(&uri, TYPE_WARNING_SOURCE);
    client.await_type_diagnostics(&uri);
}

// ── Invalid params ───────────────────────────────────────────────────────────

/// Regression (eu-oaz72.9): a request whose params did not decode
/// terminated the server.  It now gets InvalidParams and the server
/// carries on, as it does after a notification with bad params.
#[test]
fn invalid_params_answered_with_invalid_params_error() {
    let mut client = StdioClient::start(json!({}));
    client.request(1, "textDocument/hover", json!({ "bogus": true }));
    let resp = client.response_to(1);
    let err = resp.error.expect("invalid params were not rejected");
    assert_eq!(err.code, lsp_server::ErrorCode::InvalidParams as i32);

    client.notify("textDocument/didOpen", json!({ "bogus": true }));

    let uri = doc_uri("alive.eu");
    client.open(&uri, "x: 1\n");
    client.request(
        2,
        "textDocument/documentSymbol",
        json!({ "textDocument": { "uri": uri } }),
    );
    let resp = client.response_to(2);
    assert!(resp.error.is_none(), "server failed after bad params");
    let symbols = resp.result.expect("documentSymbol result");
    assert_eq!(symbols[0]["name"], "x", "unexpected symbols: {symbols}");
}

// ── Refresh requests ─────────────────────────────────────────────────────────

/// Open a document, wait for its type diagnostics, then shut down,
/// returning the methods of every server-to-client request seen before
/// the shutdown response.  The refreshes for a pipeline result are sent
/// straight after its diagnostics, so any would precede that response.
fn refresh_requests_with(capabilities: Value) -> Vec<String> {
    let mut client = StdioClient::start(capabilities);
    let uri = doc_uri("refresh.eu");
    client.open(&uri, TYPE_WARNING_SOURCE);
    let mut msgs = client.await_type_diagnostics(&uri);
    client.request(99, "shutdown", Value::Null);
    let id = RequestId::from(99);
    msgs.extend(client.collect_until(
        RESPONSE_TIMEOUT,
        |m| matches!(m, Message::Response(r) if r.id == id),
    ));
    server_request_methods(&msgs)
}

/// Regression (eu-oaz72.9): refresh requests were sent to clients that
/// had not advertised support for them.
#[test]
fn no_refresh_requests_without_client_support() {
    let methods = refresh_requests_with(json!({}));
    assert!(
        !methods
            .iter()
            .any(|m| m == INLAY_REFRESH || m == SEMANTIC_REFRESH),
        "refresh sent to a client without support: {methods:?}"
    );
}

#[test]
fn refresh_requests_sent_when_client_supports_them() {
    let methods = refresh_requests_with(json!({ "workspace": {
        "inlayHint": { "refreshSupport": true },
        "semanticTokens": { "refreshSupport": true }
    }}));
    assert!(
        methods.iter().any(|m| m == INLAY_REFRESH),
        "no inlay hint refresh: {methods:?}"
    );
    assert!(
        methods.iter().any(|m| m == SEMANTIC_REFRESH),
        "no semantic tokens refresh: {methods:?}"
    );
}

/// Each refresh is gated on its own capability.
#[test]
fn inlay_refresh_only_when_only_inlay_supported() {
    let methods = refresh_requests_with(json!({ "workspace": {
        "inlayHint": { "refreshSupport": true }
    }}));
    assert!(methods.iter().any(|m| m == INLAY_REFRESH), "{methods:?}");
    assert!(
        !methods.iter().any(|m| m == SEMANTIC_REFRESH),
        "{methods:?}"
    );
}
