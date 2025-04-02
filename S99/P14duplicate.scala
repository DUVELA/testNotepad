def duplicate[A](ls: List[A]): List[A] = ls.flatMap{i => List.fill(2)(i)}
