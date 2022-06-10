package parser

import scala.annotation.tailrec
import ParseResult.{Failure, Success}

/** Minimal parser combinator implementation.
  *
  * I would have used an existing one, but:
  *   - There didn't seem to be an obvious way of accessing a token's position in the initial input in cats parse, and
  *     the entire point of the exercice is to be able to report precisely where type errors occurred in the untyped
  *     AST.
  *   - I admitedly didn't look very hard, because this looked like fun to write.
  */
trait Parser[A]:
  /** Public API. Parses a String and returns the result. */
  def run(input: String): ParseResult[A] = run(Parser.State(input, 0))

  /** Internal API, where we keep track of things like offset in the initial input. */
  protected def run(state: Parser.State): ParseResult[A]

  /** Exposes the offset and length of the token that was found by this parser. */
  def withPosition: Parser[(A, Int, Int)] = state => run(state).withPosition

  def map[B](f: A => B): Parser[B] = str => run(str).map(f)

  def flatMap[B](f: A => Parser[B]): Parser[B] = state =>
    run(state) match
      case Failure(err, offset) => ParseResult.Failure(err, offset)
      case Success(a, rest, offset, length) =>
        f(a).run(Parser.State(rest, offset + length)).move(_ => offset).adjust(_ + length)

  /** Similar to `map`, but with the ability to fail. */
  def emap[B](f: A => Either[String, B]): Parser[B] = state => run(state).emap(f)

  /** If this parse fails, attempts the specified one instead. */
  def |(other: => Parser[A]): Parser[A] = state =>
    run(state) match
      case succ: Success[A] => succ
      case _: Failure[A]    => other.run(state)

  /** Sequences two parsers, combining their result as a tuple.
    *
    * Note that this is really just a fancy `flatMap`.
    */
  def ~[B](other: => Parser[B]): Parser[(A, B)] = for
    a <- this
    b <- other
  yield (a, b)

  def *>[B](other: => Parser[B]): Parser[B] = (this ~ other).map { case (_, b) => b }
  def <*[B](other: => Parser[B]): Parser[A] = (this ~ other).map { case (a, _) => a }

  def * : Parser[List[A]] =
    def go(state: Parser.State, acc: List[A], currLength: Int): ParseResult[List[A]] =
      run(state) match
        case Failure(err, offset) => Success(acc, state.input, offset, currLength)
        case Success(a, rest, offset, length) =>
          go(Parser.State(rest, offset + length), a :: acc, currLength + length)

    state => go(state, Nil, 0).map(_.reverse).move(_ => state.offset)

  def + : Parser[List[A]] = (this ~ this.*).map((head, tail) => head :: tail)

  def between[L, R](left: => Parser[L], right: => Parser[R]): Parser[A] = left *> this <* right

  def surroundedWith[B](p: => Parser[B]): Parser[A] = between(p, p)

object Parser:
  case class State(input: String, offset: Int)

  def pure[A](a: A): Parser[A] = state => Success(a, state.input, 0, 0)

  def char: Parser[Char] = state =>
    state.input.headOption match
      case Some(c) => Success(c, state.input.drop(1), state.offset, 1)
      case None    => Failure("Expected a character but found the empty string", state.offset)

  def char(c: Char): Parser[Char] = char.emap(c2 => Either.cond(c == c2, c, s"Expected $c but found $c2"))

  def oneOf[A](head: Parser[A], tail: Parser[A]*): Parser[A] = tail.foldLeft(head)(_ | _)

  def oneOf(head: Char, tail: Char*): Parser[Char] = oneOf(char(head), tail.map(char)*)

  def digit: Parser[Char] =
    oneOf('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  def string(str: String): Parser[String] =
    def traverse(cs: List[Char]): Parser[List[Char]] = cs match
      case head :: tail => (char(head) ~ traverse(tail)).map(_ :: _)
      case Nil          => pure(Nil)

    traverse(str.toList).map(_.mkString)

  def whitespace: Parser[Char] = oneOf(' ', '\t', '\r', '\n')

  val end: Parser[Unit] = state =>
    if state.input.isEmpty then Success((), "", 0, 0)
    else Failure(s"Expected EOS but found ${state.input}", 0)
