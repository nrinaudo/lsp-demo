package parser

/** Type class for turning a value into a list of tokens.
  *
  * Note that this is expected to be total - there must be no parsing logic here, just the act of turning something that
  * is list-like into an actual list.
  *
  * This was written specifically with the intent of allowing [[Parser]] to take a string as an input, but it might be
  * useful in other scenarios.
  */
trait Tokenizer[A, B]:
  extension (a: A) def tokenize: List[B]

object Tokenizer:
  given Tokenizer[String, Char] = _.toList

  given [A]: Tokenizer[List[A], A] = as => as
