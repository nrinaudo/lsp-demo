package abstraction

trait Semigroup[A]:
  extension (lhs: A) infix def append(rhs: A): A

object Semigroup:
  given [A]: Semigroup[List[A]] with
    extension (lhs: List[A]) infix def append(rhs: List[A]) = lhs ++ rhs
