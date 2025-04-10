def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBound = if(ls.isEmpty) 0 else n % ls.length
    if (nBound < 0) ls.takeRight(0 - nBound) ::: ls.dropRight(0 - nBound)
    else ls.drop(nBound) ::: ls.take(nBound)
}
