package parser

import scala.annotation.tailrec
import ParseResult.{Failure, Success}

/** Minimal parser combinator implementation.
  *
  * I would have used an existing one, but:
  *   - there didn't seem to be obvious ways of abstracting over the token type in existing implementations.
  *   - I admitedly didn't look very hard, because this looked like fun to write.
  *
  * `A` is the type of whatever the parser is going to parse, and `Token` the type of an atomic input. For example, a
  * String parser will have `Char` for `Token`, since the smallest possible amount of data you can read from a string is
  * a single character.
  */
trait Parser[A, Token]:
  // - Labels ----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Used in error messages to report what was being parsed in a more useful way than "expected c but found b".
  def label: String

  // Changes the parser's label.
  //
  // Note that we must both:
  // - store that new label (because some combinators rely on it).
  // - update the underlying parser's error handling, to make sure that the highest-level label we have is used.
  //
  // That's the entire trick. If we failed to update the underlying parser's error handling, errors would be triggered
  // by the "smallest" parser to fail, and use that parser's label.
  def mapLabel(f: String => String): Parser[A, Token] = Parser(
    run andThen (_.withLabel(f(label))),
    f(label)
  )

  def withLabel(name: String): Parser[A, Token] = mapLabel(_ => name)

  // - Actual parsing --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Public API. Parses anything tokenizable and returns the result.
    *
    * The [[Tokenizer]] type class is really just meant for callers to be able to pass a [[String]].
    */
  def run[Tokens](input: Tokens)(using Tokenizer[Tokens, Token]): ParseResult[A, Token] = run(
    Parser.State(input.tokenize, 0)
  )

  /** Internal API, where we keep track of things like offset in the initial input. */
  protected def run(state: Parser.State[Token]): ParseResult[A, Token]

  // - Filtering -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Fails any parser whose result doesn't match the specified predicate.
  def filter(f: A => Boolean): Parser[A, Token] = mapResult {
    case Success(value, rest, loc) if f(value) => Success(value, rest, loc)
    case Success(value, _, loc)                => Failure(s"Unexpected $value", label, loc.offset)
    case failure: Failure                      => failure
  }

  // Fails any parser whose result *does* match the specified predicate.
  def filterNot(f: A => Boolean): Parser[A, Token] = filter(f andThen (b => !b))

  // Map and filter at the same time.
  //
  // This is particularly useful when attempting to restrict a sum type to one of its branches.
  def collect[B](f: PartialFunction[A, B]): Parser[B, Token] =
    mapResult {
      case Success(a, rest, loc) =>
        f.lift(a) match
          case Some(b) => Success(b, rest, loc)
          case None    => Failure(s"Unexpected $a", label, loc.offset)
      case failure: Failure => failure
    }

  // - Mapping ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Shouldn't be used much outside of `Parser`, but is useful for many other combinators.
  def mapResult[B](f: ParseResult[A, Token] => ParseResult[B, Token]): Parser[B, Token] =
    Parser(state => f(run(state)), label)

  def map[B](f: A => B): Parser[B, Token] = mapResult(_.map(f))

  def flatMap[B](f: A => Parser[B, Token]): Parser[B, Token] = Parser(
    state =>
      run(state) match
        case Success(a, rest, Location(offset, length)) =>
          f(a)
            .mapLabel(b => s"$label and then $b")
            .run(Parser.State(rest, offset + length))
            .mapLocation(loc => Location(offset = offset, length = loc.length + length))
        case failure: Failure => failure
    ,
    label
  )

  /** Exposes the location of the token that was found by this parser. */
  def withLocation: Parser[(A, Location), Token] = mapResult(_.withLocation)

  // - Combining parsers -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  /** If this parse fails, attempts the other one instead. */
  def |(other: Parser[A, Token]): Parser[A, Token] =
    val lbl = s"$label or ${other.label}"
    Parser[A, Token](
      state =>
        run(state) match
          case succ: Success[A, Token] => succ
          case _: Failure              => other.run(state)
      ,
      lbl
    ).withLabel(lbl)

  /** Sequences two parsers, combining their result as a tuple.
    *
    * Note that this is really just a fancy `flatMap`.
    */
  def ~[B](other: => Parser[B, Token]): Parser[(A, B), Token] =
    for
      a <- this
      b <- other
    yield (a, b)

  def *>[B](other: => Parser[B, Token]): Parser[B, Token] = (this ~ other).map { case (_, b) => b }
  def <*[B](other: => Parser[B, Token]): Parser[A, Token] = (this ~ other).map { case (a, _) => a }

  /** Attempts to apply this parser 0 or more times. */
  def * : Parser[List[A], Token] =
    def go(state: Parser.State[Token], acc: List[A], currLength: Int): ParseResult[List[A], Token] =
      run(state) match
        case Failure(_, _, offset) => Success(acc, state.input, Location(offset, currLength))
        case Success(a, rest, Location(offset, length)) =>
          go(Parser.State(rest, offset + length), a :: acc, currLength + length)

    Parser(
      state => go(state, Nil, 0).map(_.reverse).mapLocation(_.copy(offset = state.offset)),
      s"list of $label"
    )

  /** Attempts to apply this parser 1 or mor times. */
  def + : Parser[List[A], Token] =
    (this ~ this.*)
      .map((head, tail) => head :: tail)
      .withLabel(s"non empty list of $label")

  def between[L, R](left: => Parser[L, Token], right: => Parser[R, Token]): Parser[A, Token] = left *> this <* right

// - Companion object --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object Parser:
  /** Used to keep track of what remains of the input, and how many tokens we've read so far. */
  case class State[Token](input: List[Token], offset: Int)

  /** Helper to remove some of the boilerplate from creating new parser instances. */
  def apply[A, Token](parser: State[Token] => ParseResult[A, Token], name: String): Parser[A, Token] =
    new Parser[A, Token]:
      override def run(state: State[Token]) = parser(state)
      override def label                    = name

  // - Basic parsers ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def pure[A, Token](a: A): Parser[A, Token] = Parser(state => Success(a, state.input, Location(0, 0)), a.toString)

  /** Parses a single token from the input. */
  def token[Token]: Parser[Token, Token] =
    val label = "token"

    Parser(
      state =>
        state.input match
          case head :: tail => Success(head, tail, Location(state.offset, 1))
          case _            => Failure("Unexpected EOF", label, state.offset)
      ,
      label
    )

  /** Attempts to parse the specified token from the input. */
  def token[Token: Eq](t: Token): Parser[Token, Token] =
    token[Token].filter(_ eq t).withLabel(t.toString)

  /** Attempts to parse the end of the input. */
  def end[Token: TokenShow]: Parser[Unit, Token] =
    val label = "EOF"

    Parser(
      state =>
        if state.input.isEmpty then Success((), List.empty, Location(0, 0))
        else Failure(s"Unexpected ${state.input.show}", label, 0),
      label
    )

  // - Combined parsers ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  def oneOf[A, Token](head: Parser[A, Token], tail: Parser[A, Token]*): Parser[A, Token] =
    def label = (head +: tail)
      .map(_.label)
      .mkString("(", " or ", ")")

    tail
      .foldLeft(head)(_ | _)
      .withLabel(label)

  def oneOf[Token: Eq](head: Token, tail: Token*): Parser[Token, Token] = oneOf(token(head), tail.map(token)*)

  // - String parsers --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def char: Parser[Char, Char] = token.withLabel("char")

  def char(c: Char): Parser[Char, Char] = token(c)

  def digit: Parser[Char, Char] = char.filter(_.isDigit).withLabel("digit")

  def whitespace: Parser[Char, Char] = char.filter(_.isWhitespace).withLabel("whitespace")

  def string(str: String): Parser[String, Char] =
    def traverse(cs: List[Char]): Parser[List[Char], Char] = cs match
      case head :: tail => (char(head) ~ traverse(tail)).map(_ :: _)
      case Nil          => pure(Nil)

    traverse(str.toList).map(_.mkString).withLabel(str)

@main def debug =
  import Parser.*

  val parser = (char('a') ~ char('b')) <* end
  println(parser.run("ac"))
