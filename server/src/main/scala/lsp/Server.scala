package lsp

import parser.LocationMap
import exp.Error
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.services.{JsonDelegate, JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.LanguageClient
import java.util.concurrent.CompletableFuture
import scala.collection.JavaConverters.*

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
class Server {

  var client: LanguageClient = null

  @JsonNotification(value = "initialized", useSegment = false)
  def initialized(params: InitializedParams): Unit = ()

  @JsonRequest(value = "initialize", useSegment = false) def initialize(
    p: InitializeParams
  ): CompletableFuture[InitializeResult] =

    val init = new CompletableFuture[InitializeResult]
    init.complete {
      val capabilities                = new ServerCapabilities()
      val workspaceServerCapabilities = new WorkspaceServerCapabilities()

      val textDocumentSyncOptions = new TextDocumentSyncOptions()
      textDocumentSyncOptions.setOpenClose(true)
      textDocumentSyncOptions.setChange(TextDocumentSyncKind.Full)
      capabilities.setTextDocumentSync(textDocumentSyncOptions)
      val serverInfo = new ServerInfo("Blang-lsp", BuildInfo.toString)

      new InitializeResult(capabilities, serverInfo)
    }
    init

  @JsonRequest(value = "shutdown", useSegment = false) def shutdown(): CompletableFuture[Object] =
    CompletableFuture[Object]()

  @JsonNotification(value = "exit", useSegment = false)
  def exit(): Unit = System.exit(0)

  @JsonNotification(value = "textDocument/didChange", useSegment = false)
  def didChange(params: DidChangeTextDocumentParams): Unit =
    client.logMessage(MessageParams(MessageType.Info, "Received didChange"))
    params.getContentChanges.asScala.headOption.map { change =>
      diagnostics(change.getText, params.getTextDocument.getUri, params.getTextDocument.getVersion)
    }

  @JsonNotification(value = "textDocument/didOpen", useSegment = false)
  def didOpen(params: DidOpenTextDocumentParams): Unit =
    client.logMessage(MessageParams(MessageType.Info, "Received didOpen"))
    diagnostics(params.getTextDocument.getText, params.getTextDocument.getUri, params.getTextDocument.getVersion)

  @JsonNotification(value = "textDocument/didClose", useSegment = false)
  def didClose(params: DidCloseTextDocumentParams): Unit =
    client.logMessage(MessageParams(MessageType.Info, "Received didClose"))

  def diagnostics(content: String, uri: String, version: Int): Unit =
    val locations = LocationMap(content)

    def range(error: Error): Range =
      val start = locations.toLocation(error.offset)
      val end = error match
        case Error.Type(_, offset, length) =>
          locations.toLocation(offset + length)
        case _: Error.Syntax =>
          start

      Range(Position(start.line, start.column), Position(end.line, end.column))

    val diagnostics = exp.Exp
      .compile(content)
      .fold(
        _.map { error =>

          val diag = Diagnostic()
          diag.setMessage(error.msg)
          diag.setSeverity(DiagnosticSeverity.Error)

          diag.setRange(range(error))
          diag
        },
        _ => List.empty
      )

    client.logMessage(MessageParams(MessageType.Info, diagnostics.toString))

    val pubDiagnostic = new PublishDiagnosticsParams(uri, diagnostics.asJava)
    pubDiagnostic.setVersion(version)
    client.publishDiagnostics(pubDiagnostic)

  def connect(cli: LanguageClient): Unit =
    client = cli
}
