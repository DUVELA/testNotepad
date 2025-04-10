def duplicateN[A](i: Int, ls: List[A]): List[A] = ls.flatMap{e => List.fill(i)(e)}
