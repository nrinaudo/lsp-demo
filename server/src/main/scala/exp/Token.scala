package exp

import parser.Location

/** All tokens one should expect to find in a source file.
  *
  * Note the presence of [[Unknown]], whose role is to try and "swallow" anything dodgy but keep parsing the file. The
  * idea is that we want to find as many unambiguous tokens as possible, to help with presenting LSP with useful
  * semantic tokens even in the face of an invalid file.
  */
enum Token:
  // We need to keep track of the token's location to:
  // - provide semantic information about it IDEs.
  // - map errors expressed in token index to character index.
  def loc: Location

  // Literals
  case Bool(value: Boolean, loc: Location)
  case Num(value: Int, loc: Location)

  // Parens
  case LeftParen(loc: Location)
  case RightParen(loc: Location)

  // Keywords
  case If(loc: Location)
  case Then(loc: Location)
  case Else(loc: Location)

  // Operators
  case Add(loc: Location)
  case Eq(loc: Location)

  // Errors
  case Unknown(token: String, loc: Location)

  override def toString = this match
    case Bool(value, _)    => value.toString
    case Num(value, _)     => value.toString
    case _: LeftParen      => "("
    case _: RightParen     => ")"
    case _: If             => "if"
    case _: Then           => "then"
    case _: Else           => "else"
    case _: Add            => "+"
    case _: Eq             => "="
    case Unknown(token, _) => token

object Token:
  def parse(input: String): Either[Error.Syntax, List[Token]] =
    tokenParser.run(input).toEither((msg, offset) => Error.Syntax(msg, Location(offset, 1)))
