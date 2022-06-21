package lsp

import org.eclipse.lsp4j.SemanticTokenTypes
import org.eclipse.lsp4j.SemanticTokenModifiers
import org.eclipse.lsp4j.SemanticTokensLegend
import scala.collection.JavaConverters.*
import scala.collection.mutable.ArrayBuilder
import exp.UntypedExp
import parser.Location
import scala.annotation.tailrec
import scala.collection.mutable.Builder
import org.eclipse.lsp4j.DocumentOnTypeFormattingOptions

/** Defines tools for turning an [[UntypedExp]] into a list of semantic tokens.
  *
  * This, as far as I can tell, is mostly useful for syntax highlighting.
  *
  * Note that this entire approach might be flawed: since an [[UntypedExp]] is needed to provide tokens for syntax
  * highlighting, it means we can't provide any for code that doesn't parse. Thoughts on how to solve this:
  *   - support incremental changes to a file. In the scenario of a file that stops parsing, we could then keep all
  *     information we computed for parts of the file that are not touched by the last edit.
  *   - have a more lenient parser for extracting semantic tokens, although this seems like a lot of work...
  */

// - Data structure ----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

case class SemanticToken(
  line: Int,
  column: Int,
  length: Int,
  tokenType: String,
  modifiers: List[String]
)

// - Encoding  ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

object SemanticToken:

  /** Encodes an entire [[UntypedExp]] to LSP semantic tokens. */
  def encode(exp: UntypedExp, map: PositionMap): List[Int] =
    encodeTokens(toTokens(exp, map))

  // - Type / Modifier to / from ints ----------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val legend: SemanticTokensLegend = SemanticTokensLegend(
    List(SemanticTokenTypes.Operator, SemanticTokenTypes.Number, SemanticTokenTypes.Keyword).asJava,
    List(SemanticTokenModifiers.Readonly).asJava
  )
  val tokenTypes: Map[String, Int]     = legend.getTokenTypes.asScala.zipWithIndex.toMap
  val tokenModifiers: Map[String, Int] = legend.getTokenModifiers.asScala.zipWithIndex.toMap

  // - Expression to tokens --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def toTokens(exp: UntypedExp, map: PositionMap): List[SemanticToken] =
    def token(loc: Location, tokenType: String, modifiers: List[String]): SemanticToken =
      val pos = map.toPosition(loc.offset)

      SemanticToken(pos.getLine, pos.getCharacter, loc.length, tokenType, modifiers)

    def go(curr: UntypedExp, acc: List[SemanticToken]): List[SemanticToken] = curr match
      case UntypedExp.Num(_, loc) =>
        token(loc, SemanticTokenTypes.Number, List(SemanticTokenModifiers.Readonly)) :: acc

      case UntypedExp.Bool(_, loc) =>
        token(loc, SemanticTokenTypes.Keyword, List(SemanticTokenModifiers.Readonly)) :: acc

      case UntypedExp.Add(lhs, rhs, loc, opLoc) =>
        val op = token(opLoc, SemanticTokenTypes.Operator, List.empty)

        go(rhs, op :: go(lhs, acc))

      case UntypedExp.Eq(lhs, rhs, loc, opLoc) =>
        val op = token(opLoc, SemanticTokenTypes.Operator, List.empty)

        go(rhs, op :: go(lhs, acc))

      case UntypedExp.Cond(cond, ifTrue, ifFalse, _, ifLoc, thenLoc, elseLoc) =>
        val ifToken   = token(ifLoc, SemanticTokenTypes.Keyword, List.empty)
        val thenToken = token(thenLoc, SemanticTokenTypes.Keyword, List.empty)
        val elseToken = token(elseLoc, SemanticTokenTypes.Keyword, List.empty)

        go(ifFalse, elseToken :: go(ifTrue, thenToken :: go(ifTrue, ifToken :: acc)))

    go(exp, List.empty)

  // - Tokens to ints --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  private def encodeModifiers(modifiers: List[String]): Option[Int] =
    modifiers.foldLeft(Option[Int](0)) { (acc, modifier) =>
      for
        curr       <- acc
        modifierId <- tokenModifiers.get(modifier)
      yield curr | (1 << modifierId)
    }

  private case class EncoderState(builder: Builder[Int, List[Int]], prevLine: Int, prevColumn: Int):
    def addToken(token: SemanticToken): EncoderState =
      // This is debatable: we chose to skip tokens whose modifiers or type are not recognised. It might be
      // better to expose them with no type or modifier information?
      val updated = for
        tokenType      <- tokenTypes.get(token.tokenType)
        tokenModifiers <- encodeModifiers(token.modifiers)
      yield
        builder += token.line - prevLine

        // Columns are only relative if on the same line as the previous token.
        if token.line == prevLine then builder += token.column - prevColumn
        else builder                           += token.column

        builder += token.length
        builder += tokenType
        builder += tokenModifiers

        copy(prevLine = token.line, prevColumn = token.column)

      updated.getOrElse(this)

  private object EncoderState:
    def empty: EncoderState = EncoderState(List.newBuilder, 0, 0)

  def encodeTokens(tokens: List[SemanticToken]): List[Int] =
    // Not entirely sure this is necessary, but it seems safer: it's possible that not all clients support
    // negative relative offsets, and sorting tokens by position in the file, ascending, ensures this can't happen.
    val sortedTokens = tokens.sortBy(token => (token.line, token.column))

    sortedTokens.foldLeft(EncoderState.empty)((state, token) => state.addToken(token)).builder.result
