package parser

import exp.UntypedExp
import scala.util.Try

/** Parses a string into an `UntypedExp`.
  *
  * This can then be pattern matched on to get an `Exp` of the desired type.
  */
val expParser: Parser[UntypedExp] =
  import Parser.*
  import UntypedExp.*

  // Literal values.
  val bool: Parser[Boolean] = string("true").map(_ => true) | string("false").map(_ => false)
  val num: Parser[Int] = digit.+.emap { digits =>
    Try(digits.mkString.toInt).toEither.left.map(_.getMessage)
  }

  // Helper for operators / keywords.
  def operator(op: String): Parser[Unit] = (whitespace.* ~ string(op) ~ whitespace.*).map(_ => ())

  // Literal expressions.
  val literal: Parser[UntypedExp] = bool.withPosition.map(Bool.apply) | num.withPosition.map(Num.apply)

  // If / then / else.
  def cond: Parser[UntypedExp] =
    ((operator("if") *> exp) ~ (operator("then") *> exp) ~ (operator("else") *> exp)).withPosition.map {
      case (((cond, ifTrue), ifFalse), offset, length) => Cond(cond, ifTrue, ifFalse, offset, length)
    }

  // Numeric equality. Note how it's defined in terms of add, which is defined in terms of paren, which is defined
  // in terms of eq. This allows us to have a relatively straightforward encoding of operator priority.
  def eq: Parser[UntypedExp] =
    val compound = ((add <* operator("=")) ~ eq).withPosition.map { case ((lhs, rhs), offset, length) =>
      Eq(lhs, rhs, offset, length)
    }
    compound | add

  // Numeric addition.
  def add: Parser[UntypedExp] =
    val compound = ((paren <* operator("+")) ~ add).withPosition.map { case ((lhs, rhs), offset, length) =>
      Add(lhs, rhs, offset, length)
    }

    compound | paren

  // Parentheses.
  def paren: Parser[UntypedExp] =
    val compound = operator("(") *> eq <* operator(")")

    compound | literal

  // "Top level" expression.
  def exp: Parser[UntypedExp] = eq | cond

  exp <* end
