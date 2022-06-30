package exp

import parser.Parser
import scala.util.{Success, Try}

/** Parser to turn a stream of characters into a stream of [[Token]]. */
private val tokenParser: Parser[Char, List[Token]] =
  import Parser.*

  type SParser[A] = Parser[Char, A]

  // - Literals --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val literal: SParser[Token] =
    val bool =
      val value = string("true").map(_ => true) | string("false").map(_ => false)
      value.withLocation.map(Token.Bool.apply)

    val num =
      val value = digit.+.map(digits => Try(digits.mkString.toInt)).collect { case Success(num) =>
        num
      }

      value.withLocation.map(Token.Num.apply)

    bool | num

  // - Operators -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val operator: SParser[Token] =
    val add = char('+').withLocation.map((_, loc) => Token.Add(loc))
    val eq  = char('=').withLocation.map((_, loc) => Token.Eq(loc))

    add | eq

  val paren: SParser[Token] =
    val lParen = char('(').withLocation.map((_, loc) => Token.LeftParen(loc))
    val rParen = char(')').withLocation.map((_, loc) => Token.RightParen(loc))

    lParen | rParen

  // - Keywords --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val keyword: SParser[Token] =
    val kIf   = string("if").withLocation.map((_, loc) => Token.If(loc))
    val kThen = string("then").withLocation.map((_, loc) => Token.Then(loc))
    val kElse = string("else").withLocation.map((_, loc) => Token.Else(loc))

    kIf | kThen | kElse

  // - Errors ----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val unknown: SParser[Token] =
    char.filterNot { (c: Char) =>
      c.isWhitespace || c == '(' || c == ')'
    }.+.map(_.mkString).withLocation.map(Token.Unknown.apply)

  // - Core parsers ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val token: SParser[Token] = literal | operator | keyword | unknown

  val sep: SParser[List[Token]] = (whitespace.map(_ => List.empty) | paren.map(p => List(p))).*.map(_.flatten)

  ((sep ~ token).map(_ :+ _).* ~ sep).map(_.flatten ++ _) <* end
