import scala.math.{sqrt, floor}

object roundCafe {
    def roundColor(x: Int, y: Int):Vector[Int] = {
        implicit val c = (sqrt(y).floor +2).toInt
        implicit val r = (x + y) / c
        Vector(r, c)
    }
}
