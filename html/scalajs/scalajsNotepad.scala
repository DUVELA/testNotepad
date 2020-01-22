//<a href="http://www.scala-js.org/doc/sjs-for-js/es6-to-scala-part2.html">scalajs part2</a>

import scala.scalajs.js.annotation._
import org.querki.jquery._
import org.scalajs.dom
import dom.document
import scala.collection.mutable

object test extends App {
    val a = mutable.Buffer("Fox", "jumped", "over")
    a.append("me") // Fox jumped over me
    a.prepend("Red") // Red Fox jumped over me
    val fox = a(1)
    a(a.length - 1) = "you" //Red fox jumped over you
    println(a.mkString(" "))

    val p = mutable.HashMap("first" -> "James", "last" -> "Phillipa")
    p("profession") = "Kid"
    val name = s"${p("first")} ${p("last")}"
    // println(name)

    val t = ("James", "Phillipa", 10)
    val kv = "key" -> 10 // same as ("key", 10)

    def sumProduct(s: Seq[Int]): (Int, Int) = {
        var sum = 0
        var product = 1
        for(i <- s) {
            sum += i
            product *= i
        }
        (sum, product)
    }

    val sc = sumProduct(Seq(1, 2, 3))
    // val sum = sc._1
    // val product = sc._2
    // with destructuring 
    val (sum, product) = sumProduct(Seq(1, 2, 3))

    val ar = Array(1, 2, 3, 4)
    val arProduct = ar.foldLeft(1)((a, x) => a * x) // foldLeft comes from WrappedArray
    // println(arProduct)

    val viewSeq = Seq(1, 2, 3, 4, 5)
    viewSeq.isEmpty == false
    viewSeq.contains(6) == false
    viewSeq.forall(x => x > 0) == true
    viewSeq.exists( x => x % 3 == 0) == true
    viewSeq.find(x => x > 3) == Some(4) // find는 리스트에서 술어함수를 만족하는 가장 첫 원소를 반환한다.
    viewSeq.head == 1
    viewSeq.tail == Seq(2, 3, 4, 5)
    viewSeq.last == 5
    viewSeq.init == Seq(1, 2, 3, 4)
    viewSeq.drop(2) == Seq(3, 4, 5)
    viewSeq.dropRight(2) == Seq(1, 2, 3)
    viewSeq.count(x => x < 3) == 2
    viewSeq.groupBy(x => x % 2) == Map(1 -> Seq(1, 2, 5), 0 -> Seq(2, 4))
    viewSeq.sortBy(x => x > 3) == Seq(5, 4, 3, 2, 1)
    viewSeq.partition(x => x > 3) == (Seq(4, 5), Seq(1, 2, 3))
    viewSeq :+ 6 == Seq(1, 2, 3, 4, 5, 6)
    viewSeq ++ Seq(6, 7) == Seq(1, 2, 3, 4, 5, 6, 7)

    def sumProductTwo(s: Seq[Int]): (Int, Int) = {
        // use a tuple accumulator to hold sum and product
        s.foldLeft((0, 1)) { case ((sum, product), x) => (sum + x, product * x)}
    }

    val m = Map("first" -> "James", "last" -> "Phillipa") 
    //m: scala.collection.immutable.Map[String,String] = Map(first -> James, last -> Phillipa)
    val data = Seq("first" -> "James", "last" -> "Phillipa")
    //data: Seq[(String, String)] = List((first,James), (last,Phillipa))
    val m2 = Map(data: _*)
    // m2: scala.collection.immutable.Map[String,String] = Map(first -> James, last -> Phillipa)

    val scores = mutable.Map.empty[String, mutable.Buffer[Int]]

    def addScore(player: String, score: Int): Unit = {
        scores.getOrElseUpdate(player, mutable.Buffer()).append(score)
    }

    def bestScore: (String, Int) = {
        val all = scores.toList.flatMap {
            case (player, pScores) => pScores.map(s => (player, s))
        }
        if (all.isEmpty)
            ("", 0)
        else
            all.maxBy(_._2)
    }

    def averageScore: Int = {
        val allScores = scores.flatMap(_._2)
        if (allScores.isEmpty)
            0
        else 
            allScores.sum / allScores.size
    }

    val set1 = Set(1, 2, 3, 4, 5)
    //set1: scala.collection.immutable.Set[Int] = Set(5, 1, 2, 3, 4)
    val set2 = Set(2, 3, 5, 1, 6)
    //set2: scala.collection.immutable.Set[Int] = Set(5, 1, 6, 2, 3)
    val addedValues = set2 diff set1 // Set(6)
    val removedValues = set1 diff set2 // Set(4)

    val common = Set("a", "the", "an", "and")
    val text = "The sun is a star and an energy source"
    val words = text.split(" ").map(_.toLowerCase).filterNot(common)
}
