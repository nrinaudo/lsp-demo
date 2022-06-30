package abstraction

trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A]) def flatMap[B](f: A => F[B]): F[B]
