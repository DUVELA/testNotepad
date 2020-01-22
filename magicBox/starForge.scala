package example

object Hello extends Greeting with App {
  println(greeting)
  def starForge(args: Int) {
    for(i <- 1 to args){
      println(s"$args" * i)
    }
  }
}

trait Greeting {
  lazy val greeting: String = "hello"
}
