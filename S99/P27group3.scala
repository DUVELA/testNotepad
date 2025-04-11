def group3For[A](ls: List[A]): List[List[List[A]]] = {
    for {
        grp1 <- combinations(2, ls)
        remaining1 = ls.diff(grp1)
        grp2 <- combinations(3, remaining1)
        remaining2 = remaining1.diff(grp2)
        grp3 <- combinations(4, remaining2)
    } yield List(grp1, grp2, grp3)
}

def groupCopilot[A](nl: List[Int], ls: List[A]): List[List[List[A]]] = nl match {
    case Nil => List(Nil)
    case n :: t => combinations(n, ls).flatMap{grp =>
        val remaining = ls.diff(grp)
        groupCopilot(t, remaining).map(grp :: _)
    }
}
