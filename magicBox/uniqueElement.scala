object Hello {
  //https://stackoverflow.com/questions/1538598/how-in-scala-to-find-unique-items-in-list
  def findUnique(xs: List[List[Int]]): List[Int] = {
        val x = xs.flatten
        x.filter(i => x.indexOf(i) == x.lastIndexOf(i))
    }
  //https://stackoverflow.com/questions/1538598/how-in-scala-to-find-unique-items-in-list

}
