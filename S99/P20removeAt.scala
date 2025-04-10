def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
    if (n < 0 || n >= ls.length) throw new NoSuchElementException(s"Invalid index $n for list of length ${ls.length}")

    val (remaining, removedElement) = (
        ls.zipWithIndex.flatMap{
            case (value, index) => if(index == n) None else Some(value)
        },
        ls.apply(n)
    )
    (remaining, removedElement)
}
