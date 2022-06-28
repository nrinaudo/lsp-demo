package parser

/** Type class used to turn a list of tokens into something human readable.
  *
  * This is designed specifically with the intent of turning a list of characters into a string, but there might be
  * other useful scenarios.
  */
trait TokenShow[A]:
  extension (tokens: List[A]) def show: String

object TokenShow:
  given TokenShow[Char] = _.mkString

  given [A]: TokenShow[A] = _.toString
