package exp

import TypedExp.Checked
import abstraction.{Semigroup, Validated}
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
  def loc: Location = this match
    case Num(_, token)                      => token.loc
    case Bool(_, token)                     => token.loc
    case Add(lhs, rhs, _)                   => lhs.loc.mergeWith(rhs.loc)
    case Eq(lhs, rhs, _)                    => lhs.loc.mergeWith(rhs.loc)
    case Cond(_, _, ifFalse, ifToken, _, _) => ifToken.loc.mergeWith(ifFalse.loc)

// - AST structure -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

  case Num(value: Int, token: Token.Num)
  case Bool(value: Boolean, token: Token.Bool)

  case Add(lhs: UntypedExp, rhs: UntypedExp, op: Token.Add)
  case Eq(lhs: UntypedExp, rhs: UntypedExp, opLoc: Token.Eq)

  case Cond(
    cond: UntypedExp,
    ifTrue: UntypedExp,
    ifFalse: UntypedExp,
    ifToken: Token.If,
    thenToken: Token.Then,
    elseToken: Token.Else
  )

  // - Type checking ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  /** Attempts to type-check the expression to the specified type. */
  def as[A](expected: Type[A]): Validated[List[Error.Type], Exp[A]] =
    typeCheck.andThen[List[Error.Type], Exp[A]] { case Checked(actual, exp) =>
      Equality.check(expected, actual) match
        case Some(Equality.Refl()) => Validated.Success(exp)
        case _                     => Validated.Failure(List(Error.Type(s"Expected $expected but found $actual", loc)))
    }

  /** Attempts to type-check the expression.
    *
    * Most of the time, [[as]] is probably a better choice, as it allows you to specify what kind of expression you're
    * expecting.
    */
  def typeCheck: Validated[List[Error.Type], TypedExp] =
    this match
      // Literals.
      case Num(value, _)  => Validated.Success(Checked(Type.Num, Exp.Num(value)))
      case Bool(value, _) => Validated.Success(Checked(Type.Bool, Exp.Bool(value)))

      // Simple binary operations.
      case Add(lhs, rhs, _) =>
        (lhs.as(Type.Num), rhs.as(Type.Num)).mapN((l, r) => Checked(Type.Num, Exp.Add(l, r)))

      case Eq(lhs, rhs, _) =>
        (lhs.as(Type.Num), rhs.as(Type.Num)).mapN((l, r) => Checked(Type.Bool, Exp.Eq(l, r)))

      case Cond(cond, ifTrue, ifFalse, _, _, _) =>
        def makeExp(c: Exp[Boolean], branches: Unify) = branches match
          case Unify.Success(tpe, l, r) =>
            Validated.Success(Checked(tpe, Exp.Cond(c, l, r)))

        (cond.as(Type.Bool), Unify.check(ifTrue, ifFalse)).mapN(Tuple2.apply).andThen(makeExp.tupled)

// - Parsing -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

object UntypedExp:

  def parse(input: List[Token]): Either[Error.Syntax, UntypedExp] =
    expParser.run(input).toEither { (err, offset) =>
      // Parsers reason in token offset, which when parsing a string maps to the index of the character in the string,
      // but we're reasoning in Token here - `offset` maps to the index of the token where the error occured. We need
      // to turn this into a position in characters.

      // Token on which the error occurred (if any - the list of tokens might be empty)
      val token = input.lift(offset).orElse(input.lastOption)

      // Location at which the error occurred. Default to the "origin" character if the list of tokens is empty.
      val loc = token.map(_.loc).getOrElse(Location(0, 0))

      Error.Syntax(err, loc)
    }
