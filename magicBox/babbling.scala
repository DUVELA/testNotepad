import scala.util.matching.Regex

object Solution {
    val patterns = "^(aya(?!.*aya)|ye(?!.*ye)|woo(?!.*woo)|ma(?!.*ma))+$".r
    def solution(babbling: Vector[String]): Int = {
        babbling.map(i => patterns.matches(i)).count(_ == true)
    }
}
