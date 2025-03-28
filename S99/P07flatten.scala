def flatten[A](ls: List[A]): List[Any] = ls match {
    case Nil => List.empty[Any]
    case ((x :: Nil) :: Nil) => x :: Nil
    case (h :: (x :: xs) :: Nil) => h :: x :: flatten(xs)
    case ((x :: xs) :: Nil) => x :: flatten(xs)
    case ((x :: Nil) :: tail) => x :: flatten(tail)
    case ((x :: xs) :: tail) => x :: flatten(xs :: flatten(tail))
    case ((Nil) :: tail) => flatten(tail)
    case (x :: tail) => x :: flatten(tail)
}
