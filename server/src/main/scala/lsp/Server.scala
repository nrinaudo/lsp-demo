package lsp

import exp.Error
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.services.{JsonDelegate, JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.LanguageClient
import java.util.concurrent.CompletableFuture
import scala.collection.JavaConverters.*
import org.eclipse.lsp4j.jsonrpc.json.ResponseJsonAdapter
import org.eclipse.lsp4j.adapters.DocumentSymbolResponseAdapter

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
class Server:

  // - State handling --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val sourceFiles: SourceCache = SourceCache()
  var client: LanguageClient   = null
  var pushDiagnostics          = true

  def connect(cli: LanguageClient): Unit =
    client = cli

  def future[A](a: => A): CompletableFuture[A] =
    val future = new CompletableFuture[A]
    future.complete(a)
    future

  // - Initialisation / shutdown ---------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  @JsonNotification(value = "initialized", useSegment = false)
  def initialized(params: InitializedParams): Unit = ()

  @JsonRequest(value = "initialize", useSegment = false) def initialize(
    p: InitializeParams
  ): CompletableFuture[InitializeResult] =
    future {
      val capabilities = new ServerCapabilities()

      val textDocumentSyncOptions = new TextDocumentSyncOptions()
      textDocumentSyncOptions.setOpenClose(true)
      textDocumentSyncOptions.setChange(TextDocumentSyncKind.Full)
      capabilities.setDiagnosticProvider(new DiagnosticRegistrationOptions(false, false))

      capabilities.setSemanticTokensProvider(
        new SemanticTokensWithRegistrationOptions(SemanticToken.legend, true, false)
      )
      capabilities.setTextDocumentSync(textDocumentSyncOptions)
      capabilities.setDocumentSymbolProvider(true)

      val serverInfo = new ServerInfo("LSP Demo", BuildInfo.toString)

      // If the client supports pull-based diagnostics, we don't need to push them.
      // Note that I'm not at all sure this is the right way of testing for that, the documentation and types are
      // a little bit unclear.
      if(p.getCapabilities.getTextDocument.getDiagnostic != null) pushDiagnostics = false

      new InitializeResult(capabilities, serverInfo)
    }

  @JsonRequest(value = "shutdown", useSegment = false) def shutdown(): CompletableFuture[Object] =
    CompletableFuture[Object]()

  @JsonNotification(value = "exit", useSegment = false)
  def exit(): Unit = System.exit(0)

  // - Document events -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // We use this to keep track of the state of all open files.

  @JsonNotification(value = "textDocument/didChange", useSegment = false)
  def didChange(params: DidChangeTextDocumentParams): Unit =
    val diags = sourceFiles.change(params.getTextDocument.getUri, params.getContentChanges.asScala.toList)

    if(pushDiagnostics)
      pushDiagnostics(diags, params.getTextDocument.getUri, params.getTextDocument.getVersion)

  @JsonNotification(value = "textDocument/didOpen", useSegment = false)
  def didOpen(params: DidOpenTextDocumentParams): Unit =
    val diags = sourceFiles.open(params.getTextDocument.getUri, params.getTextDocument.getText)

    if(pushDiagnostics)
      pushDiagnostics(diags, params.getTextDocument.getUri, params.getTextDocument.getVersion)

  @JsonNotification(value = "textDocument/didClose", useSegment = false)
  def didClose(params: DidCloseTextDocumentParams): Unit =
    sourceFiles.close(params.getTextDocument.getUri)

  // - Diagnostics -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  @JsonRequest(value = "textDocument/diagnostic", useSegment = false)
  def diagnostic(params: DocumentDiagnosticParams): CompletableFuture[DocumentDiagnosticReport] =
    future {
      DocumentDiagnosticReport(
        RelatedFullDocumentDiagnosticReport(sourceFiles.diagnostics(params.getTextDocument.getUri).asJava)
      )
    }

  def pushDiagnostics(diags: List[Diagnostic], uri: String, version: Int): Unit =
    val pubDiagnostic = new PublishDiagnosticsParams(uri, diags.asJava)
    pubDiagnostic.setVersion(version)
    client.publishDiagnostics(pubDiagnostic)

  // - Symbols ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  @JsonRequest(value = "textDocument/documentSymbol", useSegment = false)
  @ResponseJsonAdapter(classOf[DocumentSymbolResponseAdapter])
  def documentSymbol(
    params: DocumentSymbolParams
  ): CompletableFuture[java.util.List[DocumentSymbol]] =
    future {
      sourceFiles.symbols(params.getTextDocument.getUri).asJava
    }

  // - Semantic tokens -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  def semanticTokens(uri: String) =
    SemanticTokens(sourceFiles.semanticTokens(uri).map(Integer.valueOf).asJava)

  @JsonRequest(value = "textDocument/semanticTokens/full", useSegment = false)
  def semanticTokensFull(params: SemanticTokensParams): CompletableFuture[SemanticTokens] =
    future(semanticTokens(params.getTextDocument.getUri))

  @JsonRequest(value = "textDocument/semanticTokens/full/delta", useSegment = false)
  def semanticTokensFullDelta(params: SemanticTokensDeltaParams): CompletableFuture[SemanticTokens] =
    future(semanticTokens(params.getTextDocument.getUri))
