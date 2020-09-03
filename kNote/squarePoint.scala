object hello {
  def hiddenPoint(r: List[List[Int]]) = {
        val rHead = (for(i <- 0 until r.size) yield r(i).head).toList
        val rLast = (for(i <- 0 until r.size) yield r(i).last).toList
        (rHead.filter{a => rHead.indexOf(a) == rHead.lastIndexOf(a)} ::: rLast.filter{a => rLast.indexOf(a) == rLast.lastIndexOf(a)})
    }


/* fail start
  def testDdt(rf: List[Int])(acc:List[Int]): List[Int] = rf match {
        case x :: xs if(!acc.contains(x)) => testDdt(xs)(x :: acc)
        case x :: xs => testDdt(xs)(acc.filter(i => acc.indexOf(i) == acc.lastIndexOf(i)))
        case _ => acc
    }

  def xsFilter(r: List[List[Int]]): List[Int] = {
        val rf = r.flatten
        if(rf.filter(i => rf.indexOf(i) == rf.lastIndexOf(i)) != List()) rf.filter(i => rf.indexOf(i) == rf.lastIndexOf(i))
        else testDdt(r)(List())
    }
fail end*/
}
