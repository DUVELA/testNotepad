def abList(a: Int, b: Int): List[Int] = {
    if (a < b) (a to b).toList
    else (a to b by -1).toList
}

def foobar(xs: List[Int]) = {
    def inner(xs: List[Int])(acc: List[Any]): List[Any]= xs match {
      case x :: tail if (x % 2 == 0 & x % 3 == 0) => inner(tail)(acc :+ "foobar")
      case x :: tail if (x % 2 == 0) => inner(tail)(acc :+ "foo") 
      case x :: tail if (x % 3 == 0) => inner(tail)(acc :+ "bar") 
      case x :: tail => inner(tail)(acc :+ x)
      case _ => acc
    }
    inner(xs)(Nil) 
} 
