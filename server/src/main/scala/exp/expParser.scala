package exp

import scala.util.Try
import UntypedExp.{Add, Bool, Cond, Eq, Num}
import parser.{Location, ParseResult, Parser}

/** Parser to turn a stream of [[Token]] into an [[UntypedExp]]. */
private val expParser: Parser[UntypedExp, Token] =
  import Parser.*

  // - Literals --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val literal: Parser[UntypedExp, Token] = token[Token].collect {
    case token: Token.Num  => UntypedExp.Num(token.value, token)
    case token: Token.Bool => UntypedExp.Bool(token.value, token)
  }.withLabel("literal")

  // - Keywords --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val kwIf: Parser[Token.If, Token]     = token[Token].collect { case token: Token.If => token }.withLabel("if")
  val kwThen: Parser[Token.Then, Token] = token[Token].collect { case token: Token.Then => token }.withLabel("then")
  val kwElse: Parser[Token.Else, Token] = token[Token].collect { case token: Token.Else => token }.withLabel("else")

  // - Operators -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val opPlus: Parser[Token.Add, Token] = token[Token].collect { case token: Token.Add => token }.withLabel("+")
  val opEq: Parser[Token.Eq, Token]    = token[Token].collect { case token: Token.Eq => token }.withLabel("=")
  val lParen: Parser[Token.LeftParen, Token] =
    token[Token].collect { case token: Token.LeftParen => token }.withLabel("(")
  val rParen: Parser[Token.RightParen, Token] =
    token[Token].collect { case token: Token.RightParen => token }.withLabel(")")

  // - Expressions -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  // If / then / else.
  def cond: Parser[UntypedExp, Token] =
    (kwIf ~ exp ~ kwThen ~ exp ~ kwElse ~ exp).map {
      case (((((ifToken, cond), thenToken), ifTrue), elseToken), ifFalse) =>
        Cond(cond, ifTrue, ifFalse, ifToken, thenToken, elseToken)
    }.withLabel("conditional")

  // Numeric equality. Note how it's defined in terms of add, which is defined in terms of paren, which is defined
  // in terms of eq. This allows us to have a relatively straightforward encoding of operator priority.
  def eq: Parser[UntypedExp, Token] =
    val compound = (add ~ opEq ~ eq).map { case ((lhs, opLoc), rhs) =>
      Eq(lhs, rhs, opLoc)
    }.withLabel("equality")
    compound | add

  // Numeric addition.
  def add: Parser[UntypedExp, Token] =
    val compound = (paren ~ opPlus ~ add).map { case ((lhs, opLoc), rhs) =>
      Add(lhs, rhs, opLoc)
    }.withLabel("addition")

    compound | paren

  // Parentheses.
  def paren: Parser[UntypedExp, Token] =
    val compound = (lParen *> exp <* rParen).withLabel("parens")

    compound | literal

  // "Top level" expression.
  def exp: Parser[UntypedExp, Token] = eq | cond

  exp <* end
