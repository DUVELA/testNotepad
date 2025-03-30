def compress[A](ls: List[A]): List[A] = ls.foldLeft(List.empty[A]) {
    case (Nil, a) => List(a)
    case (l , a) if (l.last != a )=> l :+ a
    case (l, _) => l
}

def compressRight[A](ls: List[A]): List[A] = ls.foldRight(List[A]()){
    case (x, Nil) => List(x)
    case (x, acc) if(acc.head != x) => x :: acc
    case (_, acc) => acc
}
