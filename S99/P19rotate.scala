def rotate[A](n: Int, ls: List[A]): List[A] = {
    if (n >= 0) ls.drop(n) ::: ls.take(n)
    else ls.takeRight(0 - n) ::: ls.dropRight(0 - n)
}
