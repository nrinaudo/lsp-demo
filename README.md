# LSP demo implementation

The purpose of this project is strictly to play with LSP. Do not expect anything useful out of it.

## Running the server

Running `sbt server/assembly` should yield an `server/target/scala-3.1.3/lsp-demo-server.jar` file.

Put that, as well as `./server/src/main/sh/lsp-demo-server.sh`, in a directory on your `$PATH`.

## Testing VSCode integration

Use VSCode to open the `./vscode` directory, and open `extension.ts`. Press `F5` to debug your extension, and start typing in a new plain text file.
