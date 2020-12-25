object deviceDay {
val xs = for (line <- Source.fromFile(filename).getLines) yield line
    val xs = List (1721, 979, 366, 564, 299,  675, 1456)
    val xstureList = xs.map(x => xs.contains(2020 - x))

    val ans = for {
        i <- 0 until xs.size
        a = xstureList(i)
        if a == true
    } yield(i, xs(i)) //  IndexedSeq[(Int, Int)] = Vector((0,1721), (3,299))

     def multiple(): Unit = ans.map(i => i._2)./:(1)(_*_)
}
