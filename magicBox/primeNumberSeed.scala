package example

object Hello extends Greeting with App {
  println(greeting)
  
  def abList(a: Int, b: Int): List[Int] = {
      if (a < b) (a to b).toList
      else (a to b by -1).toList
  }
  
  def opnum(xs: List[Int]): List[Int] = {
    def inner(xs: List[Int])(acc: List[Int]): List[Int] = {
        xs match {
            case x :: tail if (x <= 1) => inner(tail)(acc) 
            case x :: tail => inner(tail.partition((i: Int) => i % x == 0)._2)(acc :+ x)
            case _ => acc
        }
    }
    inner(xs)(Nil)   
  }
}

trait Greeting {
  lazy val greeting: String = "hello"
}

// (a > b).reverse
// 1 부터 시작하면 상관 없는데 4 이상부터 시작하면 _보다 작은 값의 첫번째 배수가 나오는걸~_~
/*case x :: tail if (x == 2) => inner(tail.partition((i: Int) => i % x == 0)._2)(acc :+ x)
            case x :: tail if (x % 2 == 0) => inner(tail.partition((i: Int) => i % x == 0)._2)(acc) */
//-case x :: tail => inner(tail.partition((i: Int) => i % x == 0)._2, xs.filter((i: Int) => i % x == 0))(acc :+ x)
//inner(xs, Nil)(Nil)
//def inner(xs: List[Int], pList: List[Int])(acc: List[Int]): List[Int] = {
