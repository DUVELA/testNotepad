def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    if (n <= 0) Nil
    else {
        val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
        e :: randomSelect(n - 1, rest)
    }
}
