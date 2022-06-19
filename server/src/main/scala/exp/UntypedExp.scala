package exp

import TypedExp.Checked
import parser.Location

/** Untyped AST for our language.
  *
  * A value of this type is not directly useful: it offers no guarantee that the instructions it describes make sense.
  * It's very possible, for example, to have a valid untyped AST for `1 + (2 = 3)`, which is nonsensical.
  *
  * Having an untyped AST does allow us to parse input code and make sure it's syntactically correct before we
  * type-check the result. This is useful, as there can only be one syntax error (we'll just stop parsing when the input
  * doesn't make sense anymore) but many type ones, and we want to report as many errors as we can in one go to avoid a
  * frustrating back and forth with the end-user.
  *
  * Type checking is achieved by [[typeCheck]] or, possibly more usefully, [[as]], which also allows you to specify the
  * expected output type.
  */
enum UntypedExp:
  def loc: Location

// - AST structure -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

  case Num(value: Int, loc: Location)
  case Bool(value: Boolean, loc: Location)

  case Add(lhs: UntypedExp, rhs: UntypedExp, loc: Location, opLoc: Location)
  case Eq(lhs: UntypedExp, rhs: UntypedExp, loc: Location, opLoc: Location)

  case Cond(
    cond: UntypedExp,
    ifTrue: UntypedExp,
    ifFalse: UntypedExp,
    loc: Location,
    ifLoc: Location,
    thenLoc: Location,
    elseLoc: Location
  )

  // - Type checking ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  /** Attempts to type-check the expression to the specified type. */
  def as[A](expected: Type[A]): Either[List[Error.Type], Exp[A]] =
    typeCheck.flatMap[List[Error.Type], Exp[A]] { case Checked(actual, exp) =>
      Equality.check(expected, actual) match
        case Some(Equality.Refl()) => Right(exp)
        case _                     => Left(List(Error(s"Expected $expected but found $actual", this)))
    }

  /** Attempts to type-check the expression.
    *
    * Most of the time, [[as]] is probably a better choice, as it allows you to specify what kind of expression you're
    * expecting.
    */
  def typeCheck: Either[List[Error.Type], TypedExp] =
    this match
      // Literals.
      case Num(value, _)  => Right(Checked(Type.Num, Exp.Num(value)))
      case Bool(value, _) => Right(Checked(Type.Bool, Exp.Bool(value)))

      // Simple binary operations.
      case Add(lhs, rhs, _, _) =>
        utils.combine(lhs.as(Type.Num), rhs.as(Type.Num))((left, right) => Checked(Type.Num, Exp.Add(left, right)))

      case Eq(lhs, rhs, _, _) =>
        utils.combine(lhs.as(Type.Num), rhs.as(Type.Num))((left, right) => Checked(Type.Bool, Exp.Eq(left, right)))

      case Cond(cond, ifTrue, ifFalse, _, _, _, _) =>
        def makeExp(c: Exp[Boolean], branches: Unify) = branches match
          case Unify.Success(tpe, l, r) => Right(Checked(tpe, Exp.Cond(c, l, r)))

        utils.flatCombine(cond.as(Type.Bool), Unify.check(ifTrue, ifFalse))(makeExp)

// - Parsing -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

object UntypedExp:
  import parser.ParseResult.{Failure, Success}

  def parse(input: String): Either[Error.Syntax, UntypedExp] = expParser.run(input) match
    case Failure(err, offset) => Left(Error.Syntax(err, offset))
    case Success(exp, _, _)   => Right(exp)
