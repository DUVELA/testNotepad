object Twofer {
  // def twofer(name: String): String = "One for you, one for me."
  def twofer(name: String): String = s"One for $name, one for me."
  def twofer(): String = "One for you, one for me."
}

/*
name match { 
  case "" => "One for you, one for me."
  case _ => s"One for $name, one for me." 
}

// () != ""

name match {
  case Nil => "One for you, one for me."
  case _ => s"One for $name, one for me."
}

// pattern type is incompatible with expected type; found : scala.collection.immutable.Nil.type required: String 

*/
