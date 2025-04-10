def range(a: Int, b: Int): List[Int] = {
    if(a == b) List(a)
    else if(a < b) a :: range(a + 1, b)
    else a :: range(a - 1, b)
}
