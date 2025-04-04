def insertAt[A](a: A, n: Int, ls: List[A]): List[A] = {
    val (prefix, suffix) = ls.splitAt(n)
    prefix ::: (a :: suffix)
}
