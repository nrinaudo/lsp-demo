package abstraction

trait Applicative[F[_]] extends Functor[F]:
  extension [A](a: A) def pure: F[A]
  extension [A, B](f: F[A => B]) def ap: F[A] => F[B]

  extension [A, B, C](f: (A, B) => C)
    def lift2: (F[A], F[B]) => F[C] = (fa, fb) => fa.map(a => (b: B) => f(a, b)).ap(fb)

  extension [A, B](ab: (F[A], F[B])) def mapN[C](f: (A, B) => C): F[C] = f.lift2.apply(ab._1, ab._2)
