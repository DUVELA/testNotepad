import P09.encode
def encodeDirect[A](ls: List[A]): List[Any] = {
    encode(ls) map {t => if(t._1 == 1) t._2 else t}
}
