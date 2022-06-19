package lsp

import parser.Location
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
  private val cache: Map[String, CacheItem] = Map.empty

  private def lookup(uri: String): Option[CacheItem] = cache.synchronized {
    cache.get(uri)
  }

  // - Lifecycle events ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

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

  // - Diagnostics retrieval -------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def diagnostics(params: DocumentDiagnosticParams): List[Diagnostic] =
    lookup(params.getTextDocument.getUri).map(_.diagnostics).getOrElse(List.empty)

  // - Symbols retrieval -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private def toRange(map: PositionMap, loc: Location): Range =
    Range(map.toPosition(loc.offset), map.toPosition(loc.offset + loc.length))

  private def symbols(exp: UntypedExp, map: PositionMap): DocumentSymbol =
    val range = toRange(map, exp.loc)

    exp match
      case UntypedExp.Num(value, _) =>
        DocumentSymbol(value.toString, SymbolKind.Number, range, range)

      case UntypedExp.Bool(value, _) =>
        DocumentSymbol(value.toString, SymbolKind.Boolean, range, range)

      case UntypedExp.Add(lhs, rhs, _, opLoc) =>
        val symbol = DocumentSymbol("+", SymbolKind.Operator, range, toRange(map, opLoc))
        symbol.setChildren(List(symbols(lhs, map), symbols(rhs, map)).asJava)
        symbol

      case UntypedExp.Eq(lhs, rhs, _, opLoc) =>
        val symbol = DocumentSymbol("=", SymbolKind.Operator, range, toRange(map, opLoc))
        symbol.setChildren(List(symbols(lhs, map), symbols(rhs, map)).asJava)
        symbol

      case UntypedExp.Cond(cond, ifTrue, ifFalse, _, ifLoc, _, _) =>
        val symbol = DocumentSymbol("if", SymbolKind.Operator, range, toRange(map, ifLoc))
        symbol.setChildren(List(symbols(cond, map), symbols(ifTrue, map), symbols(ifFalse, map)).asJava)
        symbol

  def symbols(params: DocumentSymbolParams): List[DocumentSymbol] =
    lookup(params.getTextDocument.getUri).map {
      case CacheItem.Valid(_, map, exp, _) => List(symbols(exp, map))
      case _: CacheItem.Invalid            => List.empty
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
      error match
        case Error.Type(_, Location(offset, length)) => Range(map.toPosition(offset), map.toPosition(offset + length))
        case Error.Syntax(_, offset) =>
          val pos = map.toPosition(offset)
          Range(pos, pos)

    def asDiagnostic(error: Error): Diagnostic =

      val diag = Diagnostic()
      diag.setMessage(error.msg)
      diag.setSeverity(DiagnosticSeverity.Error)
      diag.setRange(range(error))

      diag

    UntypedExp.parse(source) match
      case Left(error)    => Invalid(source, map, List(asDiagnostic(error)))
      case Right(untyped) => Valid(source, map, untyped, untyped.typeCheck.left.getOrElse(List.empty).map(asDiagnostic))
