const { LanguageClient } = require('vscode-languageclient')

module.exports = {
  activate(context) {
    const executable = {
      command: 'lsp-demo-server.sh'
    }

    const serverOptions = {
      run: executable,
      debug: executable,
    }

    const clientOptions = {
      documentSelector: [{
        scheme: 'file',
        language: 'plaintext',
      }],
    }

    const client = new LanguageClient(
      'lsp-demo',
      'LSPDemo',
      serverOptions,
      clientOptions
    )

    client.start()
  },
}