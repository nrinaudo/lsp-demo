package lsp

import parser.Location
import org.eclipse.lsp4j.*
import exp.{Error, Token, TypedExp, UntypedExp}
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

  def open(uri: String, text: String): List[Diagnostic] =
    val item = CacheItem(text)
    cache.synchronized {
      cache.update(uri, item)
    }

    item.diagnostics

  def close(uri: String): Unit = cache.synchronized {
    cache.remove(uri)
  }

  // Applies the specified list of changes to the specified cached item.
  private def update(item: CacheItem, events: List[TextDocumentContentChangeEvent]): CacheItem =
    // Helper class for folding over the list of events, because I find this so much better than a tuple.
    // TODO: one way this could be improved is by keeping track of diagnostics found in the original cache item and:
    // - deleting the ones that intersect with an event.
    // - updating the location of the ones that are shifted by each patch event.
    // The result would be more accurate reporting in case of an event that doesn't yield syntactically valid code.
    case class Accumulator(source: String, map: PositionMap):
      def patch(replacement: String, range: Range) =
        val offset = map.toOffset(range.getStart)
        val length = map.toOffset(range.getEnd) - offset

        Accumulator(source.patch(offset, replacement, length))

      def replace(source: String) = Accumulator(source)
      def toCacheItem             = CacheItem(source)

    object Accumulator:
      def apply(source: String)  = Accumulator(source, PositionMap(source))
      def apply(item: CacheItem) = Accumulator(item.source, item.map)

    events
      .foldLeft(Accumulator(item)) { (acc, event) =>
        Option(event.getRange) match
          case Some(range) => acc.patch(event.getText, range)
          case None        => acc.replace(event.getText)
      }
      .toCacheItem

  def change(uri: String, changes: List[TextDocumentContentChangeEvent]): List[Diagnostic] =
    cache.synchronized {
      val updated: Option[CacheItem] = cache
        .get(uri)
        .map(item => update(item, changes))
        // If we have no cached item for the specified URI, try to find a "replace all" event, which would allow us
        // to get back on our feet. Otherwise, there really is nothing we can do.
        .orElse {
          changes.dropWhile(_.getRange != null) match
            case head :: tail => Some(update(CacheItem(head.getText), tail))
            case _            => None
        }

      updated.map { item =>
        cache.put(uri, item)
        item.diagnostics
      }.getOrElse(List.empty)
    }

  // - Diagnostics retrieval -------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def diagnostics(uri: String): List[Diagnostic] =
    lookup(uri).map(_.diagnostics).getOrElse(List.empty)

  // - Semantic tokens retrieval ---------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def semanticTokens(uri: String): List[Int] =
    lookup(uri).map {
      case CacheItem.Valid(_, map, exp, _)     => SemanticToken.encode(exp, map)
      case CacheItem.Tokens(_, map, tokens, _) => SemanticToken.encode(tokens, map)
      case _: CacheItem.Invalid                => List.empty

    }.getOrElse(List.empty)

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

      case UntypedExp.Add(lhs, rhs, op) =>
        val symbol = DocumentSymbol("+", SymbolKind.Operator, range, toRange(map, op.loc))
        symbol.setChildren(List(symbols(lhs, map), symbols(rhs, map)).asJava)
        symbol

      case UntypedExp.Eq(lhs, rhs, op) =>
        val symbol = DocumentSymbol("=", SymbolKind.Operator, range, toRange(map, op.loc))
        symbol.setChildren(List(symbols(lhs, map), symbols(rhs, map)).asJava)
        symbol

      case UntypedExp.Cond(cond, ifTrue, ifFalse, ifToken, _, _) =>
        val symbol = DocumentSymbol("if", SymbolKind.Operator, range, toRange(map, ifToken.loc))
        symbol.setChildren(List(symbols(cond, map), symbols(ifTrue, map), symbols(ifFalse, map)).asJava)
        symbol

  def symbols(uri: String): List[DocumentSymbol] =
    lookup(uri).map {
      case CacheItem.Valid(_, map, exp, _)           => List(symbols(exp, map))
      case _: (CacheItem.Invalid | CacheItem.Tokens) => List.empty
    }.getOrElse(List.empty)

// - Internals ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

private enum CacheItem:
  def source: String
  def diagnostics: List[Diagnostic]
  def map: PositionMap

  /** Source file that failed to parse. */
  case Invalid(source: String, map: PositionMap, diagnostics: List[Diagnostic])

  /** Source file that was tokenized, but couldn't be turned into an untyped exp. */
  case Tokens(source: String, map: PositionMap, tokens: List[Token], diagnostics: List[Diagnostic])

  /** Source file that parsed (but potentially failed to type-check). */
  case Valid(source: String, map: PositionMap, exp: UntypedExp, diagnostics: List[Diagnostic])

private object CacheItem:

  def apply(source: String): CacheItem =
    val map = PositionMap(source)

    def range(error: Error): Range =
      Range(map.toPosition(error.loc.offset), map.toPosition(error.loc.offset + error.loc.length))

    def asDiagnostic(error: Error): Diagnostic =

      val diag = Diagnostic()
      diag.setMessage(error.msg)
      diag.setSeverity(DiagnosticSeverity.Error)
      diag.setRange(range(error))

      diag

    Token.parse(source) match
      case Left(error)   => Invalid(source, map, List(asDiagnostic(error)))
      case Right(tokens) =>
        // If we have unknown tokens, there's no point even trying to type check. We can, however, yield interesting
        // error messages focusing on these unknown tokens.
        val unknown = tokens.collect { case Token.Unknown(token, loc) =>
          asDiagnostic(Error.Type(s"Unexpected token: $token", loc))
        }

        if unknown.nonEmpty then Tokens(source, map, tokens, unknown)
        else
          UntypedExp.parse(tokens) match
            case Left(error) => Tokens(source, map, tokens, List(asDiagnostic(error)))
            case Right(untyped) =>
              Valid(source, map, untyped, untyped.typeCheck.toEither.left.getOrElse(List.empty).map(asDiagnostic))
