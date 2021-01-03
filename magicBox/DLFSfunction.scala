import scala.math.exp

object pythonFunctionNotepad {
    def identity_function[A](x: A): A = x   //identity 가 그냥 있긴 하다.  .\.metals\readonly\scala\Predef.scala

    def sigmoid(x: Double): Double = 1 / (1 + exp(-x))
}
