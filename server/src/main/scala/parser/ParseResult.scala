package parser

/** Basically a specialised `Either` for parsing results. */
enum ParseResult[+A]:
  case Success(value: A, rest: String, offset: Int, length: Int)
  case Failure(err: String, offset: Int)

  /** "Tags" the result with its offset and length. */
  def withPosition: ParseResult[(A, Int, Int)] = this match
    case Success(value, rest, offset, length) => Success((value, offset, length), rest, offset, length)
    case Failure(err, offset)                 => Failure(err, offset)

  def map[B](f: A => B): ParseResult[B] = this match
    case Success(value, rest, offset, length) => Success(f(value), rest, offset, length)
    case Failure(err, offset)                 => Failure(err, offset)

  def emap[B](f: A => Either[String, B]): ParseResult[B] = this.map(f) match
    case Success(Right(b), rest, offset, length) => Success(b, rest, offset, length)
    case Success(Left(err), _, offset, _)        => Failure(err, offset)
    case Failure(err, offset)                    => Failure(err, offset)

  /** Moves the result's `offset` using the specified function. */
  def move(f: Int => Int): ParseResult[A] = this match
    case s: Success[A] => s.copy(offset = f(s.offset))
    case other         => other

  /** Adjusts the result's `length` using the specified function. */
  def adjust(f: Int => Int): ParseResult[A] = this match
    case s: Success[A] => s.copy(length = f(s.length))
    case other         => other
