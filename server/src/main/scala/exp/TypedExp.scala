package exp

/** Represents the result of type checking an untyped expression.
  *
  * Note how both `Type` and `Exp` are indexed on the same `T` - this allows us to pattern match on something having a
  * type of `Type.Num`, for example, and have the compiler understands that it *must* mean the corresponding expression
  * is of type `Exp[Int]`.
  */
enum TypedExp:
  case Checked[T](tpe: Type[T], exp: Exp[T])
