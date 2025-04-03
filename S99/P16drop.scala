def drop[A](i: Int, ls: List[A]): List[A] = ls.zipWithIndex.filterNot{case (_, n) => (n + 1) % 3 == 0}.map(_._1)
