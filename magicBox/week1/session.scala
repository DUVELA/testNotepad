object session extends App {
    def abs(x: Double) = if (x < 0) -x else x

    def sqrt(x: Double) = {
        def sqrtIter(guess: Double, x: Double): Double = 
            if (isGoodEnough(guess, x)) guess
            else sqrtIter(improve(guess, x), x)

        def isGoodEnough(guess: Double, x: Double) = 
            abs(guess * guess - x) / x < 0.001

        def improve(guess: Double, x: Double) = 
            (guess + x / guess) / 2
        
        sqrtIter(1.0, x)
    }
    def gcd(a: Int, b: Int): Int = 
        if (b == 0) a else gcd(b, a % b)

    def factorial(n: Int): Int = 
        if (n == 0) 1 else n * factorial(n - 1)
    
}
/** 
 def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double = 
        if (isGoodEnough(guess)) guess
        else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) = 
        abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) = 
        (guess + x / guess) / 2
    
    sqrtIter(1.0)
}
*/

/** 
def factorial(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int = {
        if (n == 0) acc
        else loop(n - 1, n * acc)
    }
    loop(n, 1)
}
*/
