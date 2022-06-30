package exp

/* AST for a fairly simple language.
 *
 * This is really only meant for demonstration purposes. There's nothing very interesting we can do with the language,
 * but getting it to work is a rather enlightening exercise.
 */
enum Exp[T]:
  // Literals.
  case Num(value: Int) extends Exp[Int]
  case Bool(value: Boolean) extends Exp[Boolean]

  // Simple binary operators.
  case Add(lhs: Exp[Int], rhs: Exp[Int]) extends Exp[Int]
  case Eq(lhs: Exp[Int], rhs: Exp[Int]) extends Exp[Boolean]

  // If / Then / Else statement.
  case Cond(cond: Exp[Boolean], ifTrue: Exp[T], ifFalse: Exp[T]) extends Exp[T]

  // Pretty printer.
  def pretty: String = this match
    case Num(value)                  => value.toString
    case Bool(value)                 => value.toString
    case Add(lhs, rhs)               => s"${lhs.pretty} + ${rhs.pretty}"
    case Eq(lhs, rhs)                => s"${lhs.pretty} = ${rhs.pretty}"
    case Cond(cond, ifTrue, ifFalse) => s"if ${cond.pretty} then ${ifTrue.pretty} else ${ifFalse.pretty}"

  // Evaluator.
  def eval: T = this match
    case Num(value)                  => value
    case Bool(value)                 => value
    case Add(lhs, rhs)               => lhs.eval + rhs.eval
    case Eq(lhs, rhs)                => lhs.eval == rhs.eval
    case Cond(cond, ifTrue, ifFalse) => if cond.eval then ifTrue.eval else ifFalse.eval

object Exp:
  /** Compiles some code into an expression.
    *
    * The return type is `TypedExp` in order to abstract over `Exp`'s type parameter. Pattern matching on the returned
    * `TypedExp` and the desired `Type` will allow you to get what you need:
    *
    * {{{
    * val intExp: Exp[Int] = compile("1 + 2") match
    *   case TypedExp.Success(Type.Num, exp) => exp
    *   case _ => sys.error("something's not right")
    * }}}
    */
  def compile(input: String): Either[List[Error], TypedExp] = for
    tokens  <- Token.parse(input).left.map(err => List(err))
    untyped <- UntypedExp.parse(tokens).left.map(err => List(err))
    typed   <- untyped.typeCheck.toEither
  yield typed
