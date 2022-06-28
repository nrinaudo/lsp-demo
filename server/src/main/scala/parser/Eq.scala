package parser

/** Basic equality type class.
  *
  * Its purpose is mostly to allow parsers such as [[Parser#char]] to exist, but it might be useful for other use cases.
  */
trait Eq[A]:
  extension (lhs: A) infix def eq(rhs: A): Boolean

object Eq:
  given Eq[Char] with
    extension (lhs: Char) infix def eq(rhs: Char) = lhs == rhs
