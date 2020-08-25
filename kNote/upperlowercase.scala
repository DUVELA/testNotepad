object Hello {
  val testCase = "path of exile".toUpperCase

  //Divide by space start
  def stringTest(s: String) = {
    for (a <- s.split(" "))
    yield a
  }
  //Divide by space end
  
  val ddt = stringTest(testCase)
  
  // repeating upper and lower start
  def ddtTest(a: Array[String]) = {
    for(i <- 0 until ddt.size)
    yield for(j <- 0 until ddt(i).size)
    yield if(j % 2 == 1) ddt(i)(j).toString.toLowerCase else ddt(i)(j)
  }
  // repeating upper and lower end
}
