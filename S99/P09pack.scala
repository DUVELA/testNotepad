  def pack[A](ls: List[A]) = {
      if(ls.isEmpty) List(List())
      else ls.foldLeft(List.empty[List[A]]){
          case (Nil, a) => List(List(a))
          case (l, a) if(l.head.contains(a)) => List(a :: l.head) ++ l.tail
          case (l, a) => List(List(a)) ++ l
      }.reverse
  }
