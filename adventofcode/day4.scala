// import scala.io.Source

object za {
    val checkPoint  = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")  

    val testCase1 = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
    val testCase4 = "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in" 
    // val testCase1 = "eyr:2027, hcl:#602927, hgt:186cm byr:1939 iyr:2019 pid:552194973 ecl:hzl"

    def inputFlatten(c: String) = c.split(":").map(i => i.split(" ")).flatten
    def checkPassport(c: String) = for(
        i <- 0 until checkPoint.size
        ) yield (inputFlatten(c)).contains(checkPoint(i))

        def emptyCheck(v: IndexedSeq[Boolean]) = for{
            i <- 0 until checkPoint.size -1  
            if checkPassport(testCase4)(i) == false // ???
        } yield List(i, checkPoint(i))

        // val filename = "./src/main/scala/example/day4.in"
        // for (line <- Source.fromFile(filename).getLines)  yield line
}
