package abstraction

trait Traverse[F[_]]:
  extension [A](fa: F[A]) def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]]

object Traverse:
  given Traverse[List] with
    def liftedCons[A, G[_]: Applicative] =
      ((head: A, tail: List[A]) => head :: tail).lift2

    extension [A](fa: List[A])
      def traverse[G[_]: Applicative, B](f: A => G[B]) = fa match
        case head :: tail => liftedCons(f(head), tail.traverse(f))
        case _            => Nil.pure
