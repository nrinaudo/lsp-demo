package utils

def combine[Error, A, B, C](left: Either[List[Error], A], right: Either[List[Error], B])(
  f: (A, B) => C
): Either[List[Error], C] = flatCombine(left, right)((l, r) => Right(f(l, r)))

def flatCombine[Error, A, B, C](left: Either[List[Error], A], right: Either[List[Error], B])(
  f: (A, B) => Either[List[Error], C]
): Either[List[Error], C] = (left, right) match
  case (Right(l), Right(r))      => f(l, r)
  case (Left(left), Left(right)) => Left(left ++ right)
  case (Left(left), _)           => Left(left)
  case (_, Left(right))          => Left(right)
