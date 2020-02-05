import scala.annotation.tailrec

def fib(n: Int): Int = {
  @tailrec 
  def fibIter(n: Int, next: Int, prev: Int): Int = {
    if (n == 0)
      prev
    else
      fibIter(n - 1, next + prev, next)
  }
  fibIter(n, 1, 0)
}
