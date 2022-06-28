package exp

import parser.Parser
import scala.util.{Success, Try}

/** Parser to turn a stream of characters into a stream of [[Token]]. */
private val tokenParser: Parser[List[Token], Char] =
  import Parser.*

  // - Literals --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val literal: Parser[Token, Char] =
    val bool: Parser[Token, Char] =
      val value = string("true").map(_ => true) | string("false").map(_ => false)
      value.withLocation.map(Token.Bool.apply)

    val num: Parser[Token, Char] =
      val value = digit.+.map(digits => Try(digits.mkString.toInt)).collect { case Success(num) =>
        num
      }

      value.withLocation.map(Token.Num.apply)

    bool | num

  // - Operators -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val operator: Parser[Token, Char] =
    val add = char('+').withLocation.map((_, loc) => Token.Add(loc))
    val eq  = char('=').withLocation.map((_, loc) => Token.Eq(loc))

    add | eq

  val paren: Parser[Token, Char] =
    val lParen = char('(').withLocation.map((_, loc) => Token.LeftParen(loc))
    val rParen = char(')').withLocation.map((_, loc) => Token.RightParen(loc))

    lParen | rParen

  // - Keywords --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val keyword: Parser[Token, Char] =
    val kIf   = string("if").withLocation.map((_, loc) => Token.If(loc))
    val kThen = string("then").withLocation.map((_, loc) => Token.Then(loc))
    val kElse = string("else").withLocation.map((_, loc) => Token.Else(loc))

    kIf | kThen | kElse

  // - Errors ----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val unknown: Parser[Token, Char] =
    char.filterNot { (c: Char) =>
      c.isWhitespace || c == '(' || c == ')'
    }.+.map(_.mkString).withLocation.map(Token.Unknown.apply)

  // - Core parsers ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val token: Parser[Token, Char] = literal | operator | keyword | unknown

  val sep: Parser[List[Token], Char] = (whitespace.map(_ => List.empty) | paren.map(p => List(p))).*.map(_.flatten)

  ((sep ~ token).map(_ :+ _).* ~ sep).map(_.flatten ++ _) <* end
