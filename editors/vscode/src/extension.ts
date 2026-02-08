import * as path from "path";
import {
  ExtensionContext,
  workspace,
} from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
  // The server command — assumes `eu` is on the PATH
  const command = workspace
    .getConfiguration("eucalypt")
    .get<string>("serverPath", "eu");

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

export function deactivate(): Thenable<void> | undefined {
  if (client) {
    return client.stop();
  }
  return undefined;
}
