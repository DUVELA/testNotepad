def B1Sum(xs: List[Int]): Int = {
    def inner(xs: List[Int], acc: Int): Int = {
        xs match {
            case x :: tail => inner(tail, acc + x)
            case Nil => acc
        }
    }
    inner(xs, 0)
}

def B1Product(ds: List[Double]): Double = {
    def inner(ds: List[Double], acc: Double): Double = {
        ds match {
            case Nil => acc
            case List(0.0) => 0
            case x :: tail => inner(tail, acc * x)
        }
    }
    inner(ds, 1)
}

def abList(a: Int, b: Int): List[Int] = {
    if (a < b) (a to b).toList
    else (a to b by -1).toList
}

def sum(a: Int, b: Int): Int =  {
    B1Sum(abList(a, b))
}

def product(a: Int, b: Int): Double = {
    B1Product(abList(a, b).map(_.toDouble))
}
