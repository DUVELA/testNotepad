// def decode[A](ls: List[(Int, A)]): List[A] = ls.map(t => List.fill(t._1)(t._2)).flatten

def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap{ e => List.fill(e._1)(e._2)}
