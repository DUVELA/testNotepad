def encodeModified[A](ls: List[A]): List[Any] = {
    import P10.encode
    encode(ls).map {case (i, c) => if(i == 1) c else (i, c)}
}
