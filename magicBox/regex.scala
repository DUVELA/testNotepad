/*
https://cnpnote.tistory.com/entry/SCALA-%EC%8A%A4%EC%B9%BC%EB%9D%BC%EC%97%90%EC%84%9C-%EC%A0%95%EA%B7%9C-%ED%91%9C%ED%98%84%EC%8B%9D%EC%9D%84-%EC%82%AC%EC%9A%A9%ED%95%98%EB%8A%94-%EB%B0%A9%EB%B2%95-%ED%8C%A8%ED%84%B4-%EC%9D%BC%EC%B9%98%EC%97%90
https://stackoverflow.com/questions/53918431/scala-regex-pattern-matching-with-string-interpolation
https://www.scala-lang.org/files/archive/api/current/scala/util/matching/Regex.html
*/

implicit class Regex(sc: StringContext) {
  def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
}
/*
scala> val(d1, d2) = "10+15" match {case r"(\d\d)${first}\+(\d\d)${second}" => (first.toInt,second.toInt) case _ => (0, 0) }
d1: Int = 10
d2: Int = 15
*/
