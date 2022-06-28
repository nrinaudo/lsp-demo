package parser

/** Basically a specialised `Either` for parsing results. */
enum ParseResult[+A, +Token]:
  // - parsed value.
  // - whatever's left to consume in the input.
  // - range (offset + length) of the original input this parsed value represented.
  case Success(value: A, rest: List[Token], loc: Location)

  // - error message
  // - label of the parser that failed. This is typically going to be "digit" to group "0 to 9", and is useful for
  //   writing meaningful error messages.
  // - offset in the original input at which the error occured.
  case Failure(err: String, label: String, offset: Int) extends ParseResult[Nothing, Nothing]

  def withLabel(label: String): ParseResult[A, Token] = this match
    case failure: Failure         => failure.copy(label = label)
    case other: Success[A, Token] => other

  /** "Tags" the result with its offset and length. */
  def withLocation: ParseResult[(A, Location), Token] = this match
    case Success(value, rest, loc) => Success((value, loc), rest, loc)
    case other: Failure            => other

  def map[B](f: A => B): ParseResult[B, Token] = this match
    case Success(value, rest, loc) => Success(f(value), rest, loc)
    case other: Failure            => other

  def mapLocation(f: Location => Location): ParseResult[A, Token] = this match
    case s: Success[A, Token] => s.copy(loc = f(s.loc))
    case other: Failure       => other

  def toEither[B](f: (String, Int) => B): Either[B, A] = this match
    case Success(value, _, _)        => Right(value)
    case Failure(err, label, offset) => Left(f(s"Error while parsing $label: $err", offset))
