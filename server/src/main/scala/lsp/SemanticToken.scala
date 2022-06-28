package lsp

import org.eclipse.lsp4j.SemanticTokenTypes
import org.eclipse.lsp4j.SemanticTokenModifiers
import org.eclipse.lsp4j.SemanticTokensLegend
import scala.collection.JavaConverters.*
import scala.collection.mutable.ArrayBuilder
import exp.{Token, UntypedExp}
import parser.Location
import scala.annotation.tailrec
import scala.collection.mutable.Builder
import org.eclipse.lsp4j.DocumentOnTypeFormattingOptions

/** Defines tools for turning an [[UntypedExp]] into a list of semantic tokens.
  *
  * This, as far as I can tell, is mostly useful for syntax highlighting.
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

  def encode(tokens: List[Token], map: PositionMap): List[Int] =
    encodeTokens(toTokens(tokens, map))

  private def token(loc: Location, map: PositionMap, tokenType: String, modifiers: List[String]): SemanticToken =
    val pos = map.toPosition(loc.offset)

    SemanticToken(pos.getLine, pos.getCharacter, loc.length, tokenType, modifiers)

  private def numToken(loc: Location, map: PositionMap): SemanticToken =
    token(loc, map, SemanticTokenTypes.Number, List(SemanticTokenModifiers.Readonly))

  private def boolToken(loc: Location, map: PositionMap): SemanticToken =
    token(loc, map, SemanticTokenTypes.Keyword, List(SemanticTokenModifiers.Readonly))

  // - Type / Modifier to / from ints ----------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  val legend: SemanticTokensLegend = SemanticTokensLegend(
    List(SemanticTokenTypes.Operator, SemanticTokenTypes.Number, SemanticTokenTypes.Keyword).asJava,
    List(SemanticTokenModifiers.Readonly).asJava
  )
  val tokenTypes: Map[String, Int]     = legend.getTokenTypes.asScala.zipWithIndex.toMap
  val tokenModifiers: Map[String, Int] = legend.getTokenModifiers.asScala.zipWithIndex.toMap

  // - Tokens to semantic tokens ---------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def toTokens(tokens: List[Token], map: PositionMap): List[SemanticToken] =
    tokens.collect {
      case Token.Bool(_, loc)    => boolToken(loc, map)
      case Token.Num(value, loc) => numToken(loc, map)
      case Token.LeftParen(loc)  => token(loc, map, SemanticTokenTypes.Operator, List.empty)
      case Token.RightParen(loc) => token(loc, map, SemanticTokenTypes.Operator, List.empty)
      case Token.If(loc)         => token(loc, map, SemanticTokenTypes.Keyword, List.empty)
      case Token.Then(loc)       => token(loc, map, SemanticTokenTypes.Keyword, List.empty)
      case Token.Else(loc)       => token(loc, map, SemanticTokenTypes.Keyword, List.empty)
      case Token.Add(loc)        => token(loc, map, SemanticTokenTypes.Operator, List.empty)
      case Token.Eq(loc)         => token(loc, map, SemanticTokenTypes.Operator, List.empty)
    }

  // - Expression to tokens --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  def toTokens(exp: UntypedExp, map: PositionMap): List[SemanticToken] =

    def go(curr: UntypedExp, acc: List[SemanticToken]): List[SemanticToken] = curr match
      case exp: UntypedExp.Num => numToken(exp.loc, map) :: acc

      case exp: UntypedExp.Bool =>
        boolToken(exp.loc, map) :: acc

      case UntypedExp.Add(lhs, rhs, opToken) =>
        val op = token(opToken.loc, map, SemanticTokenTypes.Operator, List.empty)

        go(rhs, op :: go(lhs, acc))

      case UntypedExp.Eq(lhs, rhs, opToken) =>
        val op = token(opToken.loc, map, SemanticTokenTypes.Operator, List.empty)

        go(rhs, op :: go(lhs, acc))

      case UntypedExp.Cond(cond, ifTrue, ifFalse, ifToken, thenToken, elseToken) =>
        val ifSToken   = token(ifToken.loc, map, SemanticTokenTypes.Keyword, List.empty)
        val thenSToken = token(thenToken.loc, map, SemanticTokenTypes.Keyword, List.empty)
        val elseSToken = token(elseToken.loc, map, SemanticTokenTypes.Keyword, List.empty)

        go(ifFalse, elseSToken :: go(ifTrue, thenSToken :: go(ifTrue, ifSToken :: acc)))

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
