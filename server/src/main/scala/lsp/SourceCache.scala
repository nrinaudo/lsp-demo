package lsp

import org.eclipse.lsp4j.*
import exp.{Error, TypedExp, UntypedExp}
import org.eclipse.lsp4j.Diagnostic
import collection.mutable.Map
import scala.collection.JavaConverters.*

// - Cache -------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/** Cache for all source files manipulated by an instance of LSP.
  *
  * On top of the content of the file, we'll cache diagnostics, untyped expressions... These can be used to provide
  * richer informations to clients, such as error reporting, symbol tables, semantic tokens...
  */
class SourceCache:
  val cache: Map[String, CacheItem] = Map.empty

  def open(params: DidOpenTextDocumentParams): List[Diagnostic] =
    val item = CacheItem(params.getTextDocument.getText)
    cache.synchronized {
      cache.update(params.getTextDocument.getUri, item)
    }

    item.diagnostics

  def close(params: DidCloseTextDocumentParams): Unit = cache.synchronized {
    cache.remove(params.getTextDocument.getUri)
  }

  def change(params: DidChangeTextDocumentParams): List[Diagnostic] =
    params.getContentChanges.asScala.headOption.map { change =>
      val item = CacheItem(change.getText)

      cache.synchronized {
        cache.update(params.getTextDocument.getUri, item)
      }

      item.diagnostics
    }.getOrElse(List.empty)

// - Internals ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

private enum CacheItem:
  def source: String
  def diagnostics: List[Diagnostic]
  def map: PositionMap

  /** Source file that failed to parse. */
  case Invalid(source: String, map: PositionMap, diagnostics: List[Diagnostic])

  /** Source file that parsed (but potentially failed to type-check). */
  case Valid(source: String, map: PositionMap, exp: UntypedExp, diagnostics: List[Diagnostic])

private object CacheItem:

  def apply(source: String): CacheItem =
    val map = PositionMap(source)

    def range(error: Error): Range =
      val start = map.toPosition(error.offset)
      val end = error match
        case Error.Type(_, offset, length) => map.toPosition(offset + length)
        case _: Error.Syntax               => start
      Range(start, end)

    def asDiagnostic(error: Error): Diagnostic =

      val diag = Diagnostic()
      diag.setMessage(error.msg)
      diag.setSeverity(DiagnosticSeverity.Error)
      diag.setRange(range(error))

      diag

    UntypedExp.parse(source) match
      case Left(error)    => Invalid(source, map, List(asDiagnostic(error)))
      case Right(untyped) => Valid(source, map, untyped, untyped.typeCheck.left.getOrElse(List.empty).map(asDiagnostic))
