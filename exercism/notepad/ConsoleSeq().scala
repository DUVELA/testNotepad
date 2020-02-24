scala> var intSeq: Seq[Int] = Seq()
intSeq: Seq[Int] = List()

scala> var stringSeq: Seq[String] = Seq()
stringSeq: Seq[String] = List()

scala> intSeq == stringSeq
res0: Boolean = true

============================

scala> intSeq = Seq(0xa)
intSeq: Seq[Int] = List(10)

scala> stringSeq = Seq("0xa")
stringSeq: Seq[String] = List(0xa)

scala> intSeq == stringSeq
res1: Boolean = false

============================

scala> intSeq == stringSeq.map(_.toInt)
java.lang.NumberFormatException: For input string: "0xa"

============================

scala> stringSeq = Seq("10")
stringSeq: Seq[String] = List(10)

scala> intSeq == stringSeq.map(_.toInt)
res3: Boolean = true
