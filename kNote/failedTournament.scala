import scala.annotation.tailrec
import scala.math.ceil

object Solution {
    def solution(n: Int, a: Int, b: Int): Int = {
        var answer = ddt(n, a, b)()
        answer
    }
    
    @tailrec def ddt(n: Int, a: Int, b: Int)(acc: Int = 0): Int = {
            if (n / a == n / b)
                acc 
            else ddt(n/2, ceil(a/2.0).toInt, ceil(b/2.0).toInt)(acc + 1)
        }
}
