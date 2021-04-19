def plustOne(x: Int): Int = x + 1


def my_length[T](xl: List[T]): Int = xl match {
     case Nil => 0
     case x :: xs => plustOne (my_length(xs))
}
// ex_3_2.idr my_length
