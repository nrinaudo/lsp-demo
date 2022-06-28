package exp

/* This file declares the various tools we need to interract with types at runtime. The general idea is that in order
 * to type-check something at runtime, we need to be able to manipulate types at runtime: we need values that represent
 * types. This is where `Type` comes in.
 *
 * We want to go a little bit further than that though: the output of type-checking is a typed AST, with *static* type
 * constraints. We need a way to match runtime "type as values" with compile-time static types. This is achieved with
 * `Equality`, which allows us to compare two types and, if possible, return a proof of their equality. This is
 * represented as a GADT, which allows the compiler to conclude, in the presence of `Equality[A, B]`, that `A` and `B`
 * are in fact the same type.
 *
 * Finally, there are some cases where we need to know that two expressions are of the same type, and to know what that
 * type is. This is handled by `Unify`, which can, given two `UntypedExp`s, convince the compiler that there are of the
 * same type *and* extract that type. This is, again, implemented as a GADT, so that the compiler can, in the presence
 * of a `Type[T]`, deduce that the corresponding "type as value" and `left` and `right` expressions are indexed on the
 * same type.
 */

// - Type --------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/** Represents the possible types of an expression. */
enum Type[T]:
  case Num extends Type[Int]
  case Bool extends Type[Boolean]

  override def toString() = this match
    case Num  => "number"
    case Bool => "boolean"

// - Type equality -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/** Proof that 2 types are equal. */
private enum Equality[A, B]:
  case Refl[A]() extends Equality[A, A]

private object Equality:
  def check[A, B](lhs: Type[A], rhs: Type[B]): Option[Equality[A, B]] = (lhs, rhs) match
    case (Type.Num, Type.Num)   => Some(Refl())
    case (Type.Bool, Type.Bool) => Some(Refl())
    case _                      => None

// - Expression type equality ------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/** Proof that two typed expressions are of the same type. */
private enum Unify:
  case Success[T](tpe: Type[T], left: Exp[T], right: Exp[T])

private object Unify:
  /** Attempts to type check both expressions and prove that they are of the same type. */
  def check(left: UntypedExp, right: UntypedExp): Either[List[Error.Type], Unify] =
    def unify(lhs: TypedExp, rhs: TypedExp) = (lhs, rhs) match
      case (TypedExp.Checked(lt, le), TypedExp.Checked(rt, re)) =>
        Equality.check(lt, rt) match
          case Some(Equality.Refl()) => Right(Unify.Success(lt, le, re))
          case _ =>
            Left(
              List(
                Error.Type(s"Could not unify $lt and $rt", left.loc),
                Error.Type(s"Could not unify $rt and $lt", right.loc)
              ): List[Error.Type] // Not entirely sure why this type ascription is necessary...
            )

    utils.flatCombine(left.typeCheck, right.typeCheck)(unify)
