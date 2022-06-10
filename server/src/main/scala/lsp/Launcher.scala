package lsp

import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClient

import java.io.InputStream
import java.io.OutputStream
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future

@main def main =
  val server = Server()

  val launcher = Launcher
    .Builder[LanguageClient]()
    .setInput(System.in)
    .setOutput(System.out)
    .setRemoteInterface(classOf[LanguageClient])
    .setLocalService(server)
    .create()

  val client = launcher.getRemoteProxy()

  server.connect(client)

  launcher.startListening().get()
