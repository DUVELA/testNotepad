object Hello {
  def findUnique(xs: List[List[Int]]): List[Int] = {
        val x = xs.flatten
        x.filter(i => x.indexOf(i) == x.lastIndexOf(i))
    }
  
  def hiddenPoint(r: List[List[Int]]) = {
        val rHead = (for(i <- 0 until r.size) yield r(i).head).toList
        val rLast = (for(i <- 0 until r.size) yield r(i).last).toList
        (rHead.filter{a => rHead.indexOf(a) == rHead.lastIndexOf(a)} ::: rLast.filter{a => rLast.indexOf(a) == rLast.lastIndexOf(a)})
    }
}

//https://stackoverflow.com/questions/1538598/how-in-scala-to-find-unique-items-in-list
