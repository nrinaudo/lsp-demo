package parser

/** Basically a specialised `Either` for parsing results. */
enum ParseResult[+A]:
  case Success(value: A, rest: String, loc: Location)
  case Failure(err: String, offset: Int)

  /** "Tags" the result with its offset and length. */
  def withLocation: ParseResult[(A, Location)] = this match
    case Success(value, rest, loc) => Success((value, loc), rest, loc)
    case Failure(err, offset)      => Failure(err, offset)

  def map[B](f: A => B): ParseResult[B] = this match
    case Success(value, rest, loc) => Success(f(value), rest, loc)
    case Failure(err, offset)      => Failure(err, offset)

  def emap[B](f: A => Either[String, B]): ParseResult[B] = this.map(f) match
    case Success(Right(b), rest, loc) => Success(b, rest, loc)
    case Success(Left(err), _, loc)   => Failure(err, loc.offset)
    case Failure(err, offset)         => Failure(err, offset)

  def adjustLocation(f: Location => Location): ParseResult[A] = this match
    case s: Success[A] => s.copy(loc = f(s.loc))
    case other         => other

case class Location(offset: Int, length: Int)
