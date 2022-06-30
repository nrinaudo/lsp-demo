package abstraction

/** Kind of like Either, except it doesn't stop at the first error.
  *
  * This means we cannot use monadic composition, which can be a little bit painful. This is why `andThen` is provided.
  */
enum Validated[+Error, +A]:
  case Success[A](value: A) extends Validated[Nothing, A]
  case Failure[Error](error: Error) extends Validated[Error, Nothing]

  def map[B](f: A => B): Validated[Error, B] = this match
    case Success(value) => Success(f(value))
    case Failure(error) => Failure(error)

  def andThen[E >: Error, B](f: A => Validated[E, B]): Validated[E, B] = this match
    case Success(value) => f(value)
    case Failure(error) => Failure(error)

  def toEither: Either[Error, A] = this match
    case Success(value) => Right(value)
    case Failure(error) => Left(error)

object Validated:
  given [Error: Semigroup]: Applicative[Validated[Error, _]] with
    extension [A](va: Validated[Error, A]) def map[B](f: A => B) = va.map(f)
    extension [A](a: A) def pure                                 = Validated.Success(a)
    extension [A, B](vf: Validated[Error, A => B])
      def ap = va =>
        (vf, va) match
          case (Validated.Success(f), Validated.Success(a))       => Validated.Success(f(a))
          case (Validated.Success(_), Validated.Failure(err))     => Validated.Failure(err)
          case (Validated.Failure(err), Validated.Success(_))     => Validated.Failure(err)
          case (Validated.Failure(err1), Validated.Failure(err2)) => Validated.Failure(err1 append err2)
