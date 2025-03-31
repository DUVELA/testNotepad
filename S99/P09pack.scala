def pack[A](ls: List[A]) = ls.foldLeft(List[List[A]]()){
    case (Nil, a) => List(List(a))
    case (l, a) if(l.head.contains(a)) => List(a :: l.head) ++ l.tail
    case (l, a) => List(List(a)) ++ l
}.reverse
