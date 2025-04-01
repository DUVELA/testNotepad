def encode[A](ls: List[A]): List[(Int, A)] = {
    if(ls.isEmpty) List()
    else {
        val (packed, next) = ls span { _ == ls. head}
        (packed.size, packed.head) :: encode(next)
    }
}
