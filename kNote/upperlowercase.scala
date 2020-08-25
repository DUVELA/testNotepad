object Hello {
  val testCase = "path of exile".toUpperCase

  //띄어 쓰기로 나눔 시작
  def stringTest(s: String) = {
    for (a <- s.split(" "))
    yield a
  }
  //띄어 쓰기로 나눔 끝
  
  val ddt = stringTest(testCase)
  
  // 각 대,소문자 반복 시작
  def ddtTest(a: Array[String]) = {
    for(i <- 0 until ddt.size)
    yield for(j <- 0 until ddt(i).size)
    yield if(j % 2 == 1) ddt(i)(j).toString.toLowerCase else ddt(i)(j)
  }
  // 각 대,소문자 반복 끝
}
