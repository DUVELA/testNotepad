import scala.util.matching.Regex

object Solution {
    val patterns = "^(aya|ye|woo|ma)+$".r
    def solution(babbling: Vector[String]): Int = {
        babbling.map(i => patterns.matches(i)).count(_ == true)
    }
}
