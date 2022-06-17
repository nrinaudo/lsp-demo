package lsp

import exp.Error
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.services.{JsonDelegate, JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.LanguageClient
import java.util.concurrent.CompletableFuture
import scala.collection.JavaConverters.*

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
class Server {

  // - State handling --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val sourceFiles: SourceCache = SourceCache()
  var client: LanguageClient   = null

  def connect(cli: LanguageClient): Unit =
    client = cli

  // - Initialisation / shutdown ---------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  @JsonNotification(value = "initialized", useSegment = false)
  def initialized(params: InitializedParams): Unit = ()

  @JsonRequest(value = "initialize", useSegment = false) def initialize(
    p: InitializeParams
  ): CompletableFuture[InitializeResult] =

    val init = new CompletableFuture[InitializeResult]
    init.complete {
      val capabilities = new ServerCapabilities()

      val textDocumentSyncOptions = new TextDocumentSyncOptions()
      textDocumentSyncOptions.setOpenClose(true)
      textDocumentSyncOptions.setChange(TextDocumentSyncKind.Full)

      capabilities.setTextDocumentSync(textDocumentSyncOptions)

      val serverInfo = new ServerInfo("LSP Demo", BuildInfo.toString)

      new InitializeResult(capabilities, serverInfo)
    }
    init

  @JsonRequest(value = "shutdown", useSegment = false) def shutdown(): CompletableFuture[Object] =
    CompletableFuture[Object]()

  @JsonNotification(value = "exit", useSegment = false)
  def exit(): Unit = System.exit(0)

  // - Document events -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // We use this to keep track of the state of all open files.

  @JsonNotification(value = "textDocument/didChange", useSegment = false)
  def didChange(params: DidChangeTextDocumentParams): Unit =
    val diags = sourceFiles.change(params)

    pushDiagnostics(diags, params.getTextDocument.getUri, params.getTextDocument.getVersion)

  @JsonNotification(value = "textDocument/didOpen", useSegment = false)
  def didOpen(params: DidOpenTextDocumentParams): Unit =
    val diags = sourceFiles.open(params)

    pushDiagnostics(diags, params.getTextDocument.getUri, params.getTextDocument.getVersion)

  @JsonNotification(value = "textDocument/didClose", useSegment = false)
  def didClose(params: DidCloseTextDocumentParams): Unit =
    sourceFiles.close(params)

  // - Diagnostics -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def pushDiagnostics(diags: List[Diagnostic], uri: String, version: Int): Unit =
    val pubDiagnostic = new PublishDiagnosticsParams(uri, diags.asJava)
    pubDiagnostic.setVersion(version)
    client.publishDiagnostics(pubDiagnostic)

}
