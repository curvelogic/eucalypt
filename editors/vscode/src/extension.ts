import * as path from "path";
import * as cp from "child_process";
import {
  commands,
  ExtensionContext,
  OutputChannel,
  QuickPickItem,
  window,
  workspace,
} from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

// Unicode operator table — matches the Emacs quail/transient menu
const UNICODE_OPERATORS: QuickPickItem[] = [
  { label: "∧", description: "Logical and  (&&)" },
  { label: "∨", description: "Logical or  (||)" },
  { label: "¬", description: "Logical not  (~~)" },
  { label: "≤", description: "Less-or-equal  (<=)" },
  { label: "≥", description: "Greater-or-equal  (>=)" },
  { label: "≠", description: "Not equal  (!=)" },
  { label: "∸", description: "Unary minus / negate  (/-)" },
  { label: "÷", description: "Exact division  (/%)" },
  { label: "∘", description: "Compose  (..)" },
  { label: "•", description: "Bullet / anaphor  (**)" },
  { label: "‖", description: "Cons operator  (|||)" },
  { label: "↑", description: "Head prefix  (|>)" },
  { label: "✓", description: "Non-nil check  (!!)" },
  { label: "⊕", description: "Bitwise XOR  (^^)" },
  { label: "≪", description: "Left shift  (~<)" },
  { label: "≫", description: "Right shift  (~>)" },
  { label: "∅", description: "Empty set  ({})" },
  { label: "∈", description: "Set member  (in)" },
  { label: "∉", description: "Not set member  (ni)" },
  { label: "⊝", description: "Bitwise NOT  (-o)" },
  { label: "▶", description: "Debug trace  (|=)" },
  { label: "ℕ", description: "Natural numbers  (0N)" },
  { label: "⟨", description: "Mathematical angle bracket open  ((()" },
  { label: "⟩", description: "Mathematical angle bracket close  ()))"},
  { label: "⟦", description: "Double square bracket open  ([[)" },
  { label: "⟧", description: "Double square bracket close  (]])" },
  { label: "«", description: "Left angle bracket  (<<)" },
  { label: "»", description: "Right angle bracket  (>>)" },
  { label: "⌈", description: "Ceiling bracket open  (|_)" },
  { label: "⌉", description: "Ceiling bracket close  (_|)" },
  { label: "⌊", description: "Floor bracket open  (|.)" },
  { label: "⌋", description: "Floor bracket close  (.|)" },
  { label: "‹", description: "Lens bracket open  (<.)" },
  { label: "›", description: "Lens bracket close  (.>)" },
];

async function insertUnicodeOperator(): Promise<void> {
  const editor = window.activeTextEditor;
  if (!editor) {
    return;
  }
  const picked = await window.showQuickPick(UNICODE_OPERATORS, {
    placeHolder: "Select a Unicode operator to insert",
    matchOnDescription: true,
  });
  if (picked) {
    editor.edit((editBuilder) => {
      for (const selection of editor.selections) {
        editBuilder.replace(selection, picked.label);
      }
    });
  }
}

async function renderBuffer(outputChannel: OutputChannel): Promise<void> {
  const editor = window.activeTextEditor;
  if (!editor) {
    return;
  }

  const config = workspace.getConfiguration("eucalypt");
  const euCommand = config.get<string>("euCommand", "eu");
  const globalOpts = config.get<string>("globalOptions", "");

  const filePath = editor.document.fileName;
  const ext = path.extname(filePath).slice(1) || "eu";
  const fmt = ext === "yaml" ? "yaml" : ext === "csv" ? "csv" : "eu";
  const args = [euCommand, ...globalOpts.split(" ").filter(Boolean), `${fmt}@-`];

  outputChannel.clear();
  outputChannel.show(true);

  const text = editor.document.getText();
  const proc = cp.spawn(args[0], args.slice(1), { shell: false });

  let stdout = "";
  let stderr = "";
  proc.stdout.on("data", (d: Buffer) => { stdout += d.toString(); });
  proc.stderr.on("data", (d: Buffer) => { stderr += d.toString(); });
  proc.stdin.write(text);
  proc.stdin.end();

  proc.on("close", (code) => {
    if (code === 0) {
      outputChannel.append(stdout);
    } else {
      outputChannel.append(stderr || stdout);
      window.showErrorMessage(`eu exited with code ${code}`);
    }
  });

  proc.on("error", (err) => {
    window.showErrorMessage(`Failed to start eu: ${err.message}`);
  });
}

export function activate(context: ExtensionContext) {
  const config = workspace.getConfiguration("eucalypt");

  // Create output channel once so it is reused across renderBuffer invocations
  const outputChannel = window.createOutputChannel("Eucalypt");
  context.subscriptions.push(outputChannel);

  // Register commands
  context.subscriptions.push(
    commands.registerCommand(
      "eucalypt.insertUnicodeOperator",
      insertUnicodeOperator
    )
  );

  context.subscriptions.push(
    commands.registerCommand("eucalypt.renderBuffer", () =>
      renderBuffer(outputChannel)
    )
  );

  // Start the LSP client only when enabled (default: true)
  const lspEnabled = config.get<boolean>("lspEnabled", true);
  if (lspEnabled) {
    const command = config.get<string>("euCommand", "eu");

    const serverOptions: ServerOptions = {
      command,
      args: ["lsp"],
    };

    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: "file", language: "eucalypt" }],
    };

    client = new LanguageClient(
      "eucalypt",
      "Eucalypt Language Server",
      serverOptions,
      clientOptions
    );

    client.start();
  }
}

export function deactivate(): Thenable<void> | undefined {
  if (client) {
    return client.stop();
  }
  return undefined;
}
