package exp

/* Declares all supported error types.
 *
 * Syntax errors occur at parse time. If the parser finds a character it's not expecting, it'll just report it and
 * stop there.
 *
 * Type errors occur during type checking. If the type-checker finds types that don't match it's expectations, it'll
 * report them, but keep type-checking everything it still can.
 *
 * All error types are tagged with the part of the input code that causes the problem.
 */
enum Error:
  case Syntax(msg: String, offset: Int)
  case Type(msg: String, offset: Int, length: Int)

  def msg: String
  def offset: Int

object Error:
  def apply(msg: String, exp: UntypedExp): Error.Type = Error.Type(msg, exp.offset, exp.length)
