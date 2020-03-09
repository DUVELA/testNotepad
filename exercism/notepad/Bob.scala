object Bob {
  def response(sayHo: String): String = sayHo.trim match {
    case s if s.isEmpty => "Fine. Be that way!"
    case s if s.endsWith("?") && yellCase(s) => "Calm down, I know what I'm doing!"
    case s if yellCase(s) => "Whoa, chill out!"
    case s if s.endsWith("?") => "Sure."
    case _ => "Whatever."
  }
  
  def yellCase(str: String) = str.equals(str.toUpperCase) && !str.replaceAll("[^a-zA-Z0-9]","").forall(s => s.isDigit)
}
/*
//trim
scala> "asdf123!@#   !@#123asd    ".trim.length
res7: Int = 22

scala> "asdf123!@#   !@#123asd    ".length
res9: Int = 26
============================================
scala> val bobSureOne = "Okay if like my  spacebar  quite a bit?   "
bobSureOne: String = "Okay if like my  spacebar  quite a bit?   "

scala> if(bobSureOne.trim.last == '?') "Sure" else "Whatever"
res0: String = Sure

scala> if(bobSureOne.trim.endsWith("?")) "Sure" else "Whatever"
res1: String = Sure

scala> bobSureOne.equals(bobSureOne.toUpperCase)
res2: Boolean = false
============================================
scala> "asdf123!@#   !@#123asd".replaceAll("[^a-zA-Z0-9]","")
res3: String = asdf123123asd

scala> "123   123    ".replaceAll("[^a-zA-Z0-9]","")
res12: String = 123123

scala> "123   123    ".replaceAll("[^a-zA-Z0-9]","").forall(s => s.isDigit)
res13: Boolean = true

scala> "123   123    ".forall(s => s.isDigit)
res14: Boolean = false

// def yellCase(str: String) = str.equals(str.toUpperCase) && !asd.replaceAllIn(str,"").forall(s => s.isDigit)
// val asd = "[^a-zA-Z0-9]".r
*/
