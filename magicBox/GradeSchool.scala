import scala.collection.mutable
import scala.collection.immutable.SortedMap


class School {
  type DB = Map[Int, Seq[String]]
  private val basicDB = mutable.Map[Int, Seq[String]]()

  def add(name: String, g: Int) = basicDB += (g -> {grade(g) :+ name}) 

  def db: DB = Map(basicDB.toSeq: _*)

  def grade(g: Int): Seq[String] = basicDB.getOrElse(g, Seq.empty[String])
  
  def sorted: DB = SortedMap(db.toSeq : _*).map{case (a: Int,b: Seq[String]) => (a, b.sorted)}
}
