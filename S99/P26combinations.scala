def combinations[A](n: Int, ls: List[A]): List[List[A]] = (n, ls) match {
    case (_, Nil) => List.empty[List[A]]
    case (1, _) => ls.map(List(_))
    case (_, h :: t) => combinations(n - 1, t).map(h :: _) ::: combinations(n, t)
}
