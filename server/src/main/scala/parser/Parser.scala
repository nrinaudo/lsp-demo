package parser

import scala.annotation.tailrec
import ParseResult.{Failure, Success}
import abstraction.*

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
trait Parser[Token, A]:
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
  def mapLabel(f: String => String): Parser[Token, A] = Parser(
    run andThen (_.withLabel(f(label))),
    f(label)
  )

  def withLabel(name: String): Parser[Token, A] = mapLabel(_ => name)

  // - Actual parsing --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Public API. Parses anything tokenizable and returns the result.
    *
    * The [[Tokenizer]] type class is really just meant for callers to be able to pass a [[String]].
    */
  def run[Tokens](input: Tokens)(using Tokenizer[Tokens, Token]): ParseResult[Token, A] = run(
    Parser.State(input.tokenize, 0)
  )

  /** Internal API, where we keep track of things like offset in the initial input. */
  protected def run(state: Parser.State[Token]): ParseResult[Token, A]

  // - Filtering -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Fails any parser whose result doesn't match the specified predicate.
  def filter(f: A => Boolean): Parser[Token, A] = mapResult {
    case Success(value, rest, loc) if f(value) => Success(value, rest, loc)
    case Success(value, _, loc)                => Failure(s"Unexpected $value", label, loc.offset)
    case failure: Failure                      => failure
  }

  // Fails any parser whose result *does* match the specified predicate.
  def filterNot(f: A => Boolean): Parser[Token, A] = filter(f andThen (b => !b))

  // Map and filter at the same time.
  //
  // This is particularly useful when attempting to restrict a sum type to one of its branches.
  def collect[B](f: PartialFunction[A, B]): Parser[Token, B] =
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
  def mapResult[B](f: ParseResult[Token, A] => ParseResult[Token, B]): Parser[Token, B] =
    Parser(state => f(run(state)), label)

  def map[B](f: A => B): Parser[Token, B] = mapResult(_.map(f))

  def flatMap[B](f: A => Parser[Token, B]): Parser[Token, B] = Parser(
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
  def withLocation: Parser[Token, (A, Location)] = mapResult(_.withLocation)

  // - Combining parsers -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  /** If this parse fails, attempts the other one instead. */
  def |(other: Parser[Token, A]): Parser[Token, A] =
    val lbl = s"$label or ${other.label}"
    Parser[Token, A](
      state =>
        run(state) match
          case succ: Success[Token, A] => succ
          case _: Failure              => other.run(state)
      ,
      lbl
    ).withLabel(lbl)

  /** Sequences two parsers, combining their result as a tuple.
    *
    * Note that this is really just a fancy `flatMap`.
    */
  def ~[B](other: => Parser[Token, B]): Parser[Token, (A, B)] =
    for
      a <- this
      b <- other
    yield (a, b)

  def *>[B](other: => Parser[Token, B]): Parser[Token, B] = (this ~ other).map { case (_, b) => b }
  def <*[B](other: => Parser[Token, B]): Parser[Token, A] = (this ~ other).map { case (a, _) => a }

  /** Attempts to apply this parser 0 or more times. */
  def * : Parser[Token, List[A]] =
    def go(state: Parser.State[Token], acc: List[A], currLength: Int): ParseResult[Token, List[A]] =
      run(state) match
        case Failure(_, _, offset) => Success(acc, state.input, Location(offset, currLength))
        case Success(a, rest, Location(offset, length)) =>
          go(Parser.State(rest, offset + length), a :: acc, currLength + length)

    Parser(
      state => go(state, Nil, 0).map(_.reverse).mapLocation(_.copy(offset = state.offset)),
      s"list of $label"
    )

  /** Attempts to apply this parser 1 or mor times. */
  def + : Parser[Token, List[A]] =
    (this ~ this.*)
      .map((head, tail) => head :: tail)
      .withLabel(s"non empty list of $label")

  def between[L, R](left: => Parser[Token, L], right: => Parser[Token, R]): Parser[Token, A] = left *> this <* right

// - Companion object --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object Parser:
  /** Used to keep track of what remains of the input, and how many tokens we've read so far. */
  case class State[Token](input: List[Token], offset: Int)

  /** Helper to remove some of the boilerplate from creating new parser instances. */
  def apply[Token, A](parser: State[Token] => ParseResult[Token, A], name: String): Parser[Token, A] =
    new Parser[Token, A]:
      override def run(state: State[Token]) = parser(state)
      override def label                    = name

  // - Abstractions ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  given [Token]: Monad[Parser[Token, _]] with
    extension [A](fa: Parser[Token, A]) def map[B](f: A => B)                    = fa.map(f)
    extension [A](a: A) def pure                                                 = Parser.pure(a)
    extension [A, B](f: Parser[Token, A => B]) def ap                            = Parser.ap(f)
    extension [A](fa: Parser[Token, A]) def flatMap[B](f: A => Parser[Token, B]) = fa.flatMap(f)

  def pure[A, Token](a: A): Parser[Token, A] = Parser(state => Success(a, state.input, Location(0, 0)), a.toString)

  def ap[A, B, Token](pf: Parser[Token, A => B]): Parser[Token, A] => Parser[Token, B] = pa =>
    for
      f <- pf
      a <- pa
    yield f(a)

  // - Basic parsers ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
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
  def end[Token: TokenShow]: Parser[Token, Unit] =
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

  def string(str: String): Parser[Char, String] =
    import abstraction.Traverse.given

    str.toList.traverse(char).map(_.mkString).withLabel(str)
