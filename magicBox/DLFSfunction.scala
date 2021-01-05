import scala.math.exp

object pythonFunctionNotepad {
    def identity_function[A](x: A): A = x   //identity 가 그냥 있긴 하다.  .\.metals\readonly\scala\Predef.scala

    def sigmoid(x: Double): Double = 1 / (1 + exp(-x))

    def oneOrZero(x: Double): Double = if(x > 0 ) 1 else 0

    def checkTwoType[A](x: A) = x match {
        case x: Double => oneOrZero(x)
        case x: List[Double] => x.map(oneOrZero(_))
    }

    def sigmoid_grad(x: Double) = (1.0 - sigmoid(x)) * sigmoid(x)

    def max(a: Double = 0, b: Double) = if(a > b) a else b
    def relu[A](x: A) = x match {
        case x: Double => max(0, x)
        case x: List[Double] => x.map(max(0, _))
    }

    def relu_grade(x: Int): List[Int] = List.fill(x)(1) //#1

}
