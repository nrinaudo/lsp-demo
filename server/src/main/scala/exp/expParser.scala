package exp

import scala.util.Try
import UntypedExp.{Add, Bool, Cond, Eq, Num}
import parser.{Location, ParseResult, Parser}

/** Parser to turn a stream of [[Token]] into an [[UntypedExp]]. */
private val expParser: Parser[Token, UntypedExp] =
  import Parser.*

  type TParser[A] = Parser[Token, A]

  // - Literals --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val literal: TParser[UntypedExp] = token[Token].collect {
    case token: Token.Num  => UntypedExp.Num(token.value, token)
    case token: Token.Bool => UntypedExp.Bool(token.value, token)
  }.withLabel("literal")

  // - Keywords --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val kwIf: TParser[Token.If]     = token[Token].collect { case token: Token.If => token }.withLabel("if")
  val kwThen: TParser[Token.Then] = token[Token].collect { case token: Token.Then => token }.withLabel("then")
  val kwElse: TParser[Token.Else] = token[Token].collect { case token: Token.Else => token }.withLabel("else")

  // - Operators -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val opPlus: TParser[Token.Add]        = token[Token].collect { case token: Token.Add => token }.withLabel("+")
  val opEq: TParser[Token.Eq]           = token[Token].collect { case token: Token.Eq => token }.withLabel("=")
  val lParen: TParser[Token.LeftParen]  = token[Token].collect { case token: Token.LeftParen => token }.withLabel("(")
  val rParen: TParser[Token.RightParen] = token[Token].collect { case token: Token.RightParen => token }.withLabel(")")

  // - Expressions -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  // If / then / else.
  def cond: TParser[UntypedExp] =
    (kwIf ~ exp ~ kwThen ~ exp ~ kwElse ~ exp).map {
      case (((((ifToken, cond), thenToken), ifTrue), elseToken), ifFalse) =>
        Cond(cond, ifTrue, ifFalse, ifToken, thenToken, elseToken)
    }.withLabel("conditional")

  // Numeric equality. Note how it's defined in terms of add, which is defined in terms of paren, which is defined
  // in terms of eq. This allows us to have a relatively straightforward encoding of operator priority.
  def eq: TParser[UntypedExp] =
    val compound = (add ~ opEq ~ eq).map { case ((lhs, opLoc), rhs) =>
      Eq(lhs, rhs, opLoc)
    }.withLabel("equality")
    compound | add

  // Numeric addition.
  def add: TParser[UntypedExp] =
    val compound = (paren ~ opPlus ~ add).map { case ((lhs, opLoc), rhs) =>
      Add(lhs, rhs, opLoc)
    }.withLabel("addition")

    compound | paren

  // Parentheses.
  def paren: TParser[UntypedExp] =
    val compound = (lParen *> exp <* rParen).withLabel("parens")

    compound | literal

  // "Top level" expression.
  def exp: TParser[UntypedExp] = eq | cond

  exp <* end
