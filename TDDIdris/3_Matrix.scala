//Ai로 만든 3_Matrix.idr의 transposeMat. 이름 오타는 작성자 이슈.
object transpose {
    def transposeMat(matrix: Vector[Vector[Double]]): Vector[Vector[Double]] = {
        def createEmpties(column: Vector[Vector[Double]]) = Vector.fill(column.head.length)(Vector.empty[Double]) // fill이 idris2의 replicate와 비슷한 역할을 한다. 

        def transposeHelper (row: Vector[Double], tailMatrix: Vector[Vector[Double]]) : Vector[Vector[Double]] = (row, tailMatrix) match {
            case (Vector(), Vector()) => Vector.empty
            case (x +: xs, y +: ys) => (y :+ x) +: transposeHelper(xs, ys)
            case _ => throw new IllegalArgumentException("Row and tailMatrix length mismatch")
        }

        def loop(matrix: Vector[Vector[Double]], acc: Vector[Vector[Double]]): Vector[Vector[Double]] = matrix match {
            case Vector()   => acc
            case x +: xs    => loop(xs, transposeHelper(x, acc))
            case _ => throw new IllegalArgumentException("Matrix and acc length mismatch")
        }
        loop(matrix, createEmpties(matrix))
    }
    val m33 = Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0),
        Vector(7.0, 8.0, 9.0)
    )

    val m23 = Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0)
    )
}
