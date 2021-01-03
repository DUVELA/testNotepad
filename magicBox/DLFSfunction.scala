import scala.math.exp

object pythonFunctionNotepad {
    def identity_function[A](x: A): A = x   // .\.metals\readonly\scala\Predef.scala

    def sigmoid(x: Double): Double = 1 / (1 + exp(-x))
}
