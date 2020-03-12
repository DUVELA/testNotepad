import scala.annotation.tailrec
object Etl {
    val onePoints = "[AEIOULNRST]".r
    val twoPoints = "[D,G]".r
    val threePoints = "[BCMP]".r
    val fourPoints = "[FHVWY]".r
    val fivePoint = "[K]".r
    val eightPoints = "[JX]".r
    val tenPoints = "[QZ]".r

    
    def transform(input: Map[Int, Seq[String]]) = {
        val stone = input.values.flatten.toList
        b1Transform(stone)(Map())
    }

    @tailrec
    def b1Transform(element: List[String])(acc: Map[String, Int]): Map[String, Int] = element match {
        case xh :: xt if!(xh.equals(fivePoint.replaceAllIn(xh, ""))) => b1Transform(xt)(acc ++ Map(xh.toLowerCase -> 5))
        case xh :: xt if!(xh.equals(eightPoints.replaceAllIn(xh, ""))) => b1Transform(xt)(acc ++ Map(xh.toLowerCase -> 8))
        case xh :: xt if!(xh.equals(tenPoints.replaceAllIn(xh, ""))) => b1Transform(xt)(acc ++ Map(xh.toLowerCase -> 10))
        case xh :: xt if!(xh.equals(twoPoints.replaceAllIn(xh, ""))) => b1Transform(xt)(acc ++ Map(xh.toLowerCase -> 2))
        case xh :: xt if!(xh.equals(threePoints.replaceAllIn(xh, ""))) => b1Transform(xt)(acc ++ Map(xh.toLowerCase -> 3))
        case xh :: xt if!(xh.equals(fourPoints.replaceAllIn(xh, ""))) => b1Transform(xt)(acc ++ Map(xh.toLowerCase -> 4))
        case xh :: xt if!(xh.equals(onePoints.replaceAllIn(xh, ""))) => b1Transform(xt)(acc ++ Map(xh.toLowerCase -> 1))
        case Nil => acc
    }
}
