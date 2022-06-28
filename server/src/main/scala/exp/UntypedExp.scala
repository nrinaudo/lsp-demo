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
  def as[A](expected: Type[A]): Either[List[Error.Type], Exp[A]] =
    typeCheck.flatMap[List[Error.Type], Exp[A]] { case Checked(actual, exp) =>
      Equality.check(expected, actual) match
        case Some(Equality.Refl()) => Right(exp)
        case _                     => Left(List(Error.Type(s"Expected $expected but found $actual", loc)))
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
      case Add(lhs, rhs, _) =>
        utils.combine(lhs.as(Type.Num), rhs.as(Type.Num))((left, right) => Checked(Type.Num, Exp.Add(left, right)))

      case Eq(lhs, rhs, _) =>
        utils.combine(lhs.as(Type.Num), rhs.as(Type.Num))((left, right) => Checked(Type.Bool, Exp.Eq(left, right)))

      case Cond(cond, ifTrue, ifFalse, _, _, _) =>
        def makeExp(c: Exp[Boolean], branches: Unify) = branches match
          case Unify.Success(tpe, l, r) => Right(Checked(tpe, Exp.Cond(c, l, r)))

        utils.flatCombine(cond.as(Type.Bool), Unify.check(ifTrue, ifFalse))(makeExp)

// - Parsing -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

object UntypedExp:

  def parse(input: List[Token]): Either[Error.Syntax, UntypedExp] =
    expParser.run(input).toEither { (err, offset) =>
      // Token on which the error occurred.
      val token = input.lift(offset).getOrElse(input.last)

      // Since parsers reason in token offset, `offset`, here, is the offset of the token in which the error occurred,
      // not of the *character* in which it occurred. We need to map from the former to the later.
      Error.Syntax(err, token.loc)
    }
