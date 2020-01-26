import scala.scalajs.js.annotation._
import org.querki.jquery._
import org.scalajs.dom
import dom.ext.Ajax
import dom.document
import scala.annotation.tailrec
import scalajs.js
import org.scalajs.dom.raw._
// import org.scalajs.dom.raw.DOMList
import org.scalajs.dom.ext._



case class Person(first: String, last: String, age: Int)
case class Date(year: Int, month: Int, day: Int)
case class Joined(name: String, joined: js.Date)

object es6Advanced extends App {
    def prinType(o: Any): Unit = {
        o match {
            case s: String =>
                println(s"It's a String: $s")
            case i: Int =>    
                println(s"It's an Int: $i")
            case b: Boolean =>
                println(s"It's a boolean: $b")
            case _ =>
                println("It's something else")    
        }
    }

    def parse(str: String, magicKey: Char): Seq[String] = {
        str.map {
            case c if c == magicKey =>
                "magic"
            case c if c.isDigit =>
                "digit"
            case c if c.isLetter =>
                "letter"
            case c if c.isWhitespace =>
                " "
            case c =>
                "char"
        }
    }
    val r = parse("JB/007", '/')

    //case class Person
    val person = Person("James", "phillipa", 15)
    val newPerson = new Person("James", "Bond", 42)
    val seq = Seq(1, 2, 3, 4, 5)
    val Seq(a, b, _, c @ _*) = seq
    val seq2 = Seq(a, b) ++ c

    def ageSum(persons: Seq[Person], family: String): Int = {
        persons.collect {
            case Person(_, last, age) if last == family =>
                age
        }.sum
    }
    val persons = Seq(Person("James", "Bond", 42),Person("Hillary", "Bond", 35), Person("James", "Pillipa", 15))
    println(ageSum(persons, "Bond") == 77)
    // case class Date
    def convertToDate(d: String): Date = {
        val YMD = """(\d{4})-(\d{1,2})-(\d{1,2})""".r
        val MDY = """(\d{1,2})/(\d{1,2})/(\d{4})""".r
        val DMY = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r
        d match {
            case YMD(year, month, day) =>
                Date(year.toInt, month.toInt, day.toInt)
            case MDY(month, day, year) =>
                Date(year.toInt, month.toInt, day.toInt)
            case DMY(day, month, year) =>
                Date(year.toInt, month.toInt, day.toInt)
            case _ =>
                throw new Exception("Invalid date!")
        }
    }
    // convertToDate("2015-10-9") // = Date(2015,10,9)
    // convertToDate("10/9/2015") // = Date(2015,10,9)
    // convertToDate("9.10.2015") // = Date(2015,10,9)
    // convertToDate("10 Nov 2015") // exception

    def minmaxBy[T](seq: Seq[T])(f: T => Int): (Int, Int) = {
        seq.foldLeft((Int.MaxValue, Int.MinValue)) {
            case ((min, max), e) =>
                val v = f(e)
                (math.min(min, v), math.max(max, v))
        }
    }
    val (youngest, oldest) = minmaxBy[Person](persons)(_.age)

    def compute(value: Int, cPos: => Int, cNeg: => Int) = {
        if (value >= 0)
            cPos
        else
            cNeg
    }
    // compute(x, expCalc, expCalc2)

    def fib(n: Int): Int = {
        @annotation.tailrec
        def fibIter(n: Int, next: Int, prev: Int): Int = {
            if (n == 0)
                prev
            else
            fibIter(n - 1, next + prev, next)
        }
        fibIter(n, 1, 0)
    }

    def tag(name: String)(content: String): String = {
        s"<$name>$content</$name>"
    }

    val div = tag("div") _
    val p = tag("p") _
    val html = div(p("test"))

    implicit def convertFromJSDate(d: js.Date): java.util.Date = {
        new java.util.Date(d.getMilliseconds())
    }

    implicit def convertToJSDate(d: java.util.Date): js.Date = {
        new js.Date(d.getTime)
    }

    //case class Joined
    val jp = Joined("James Pillipa", new java.util.Date)

    implicit class StrToDate(val s: String) {
        def toDate = convertToDate(s)
    }
    "2015-10-09".toDate

    implicit class NodeListSeq[T <: Node](nodes: DOMList[T]) extends IndexedSeq[T] {
    
        override def foreach[U](f: T => U): Unit = {
            for (i <- 0 until nodes.length) {
                f(nodes(i))
            }
        }
        override def length: Int = nodes.length
        override def apply(idx: Int): T = nodes(idx)
    }

    val images = dom.document.querySelectorAll("img").asInstanceOf[NodeListOf[HTMLImageElement]]
    val urls = images.map(i => i.src)
    val withClass = images.filter(i => i.className.nonEmpty)
    images.sortBy(i => -i.width).take(10).foreach { i =>
        i.onclick = (e: MouseEvent) => println("Image clicked!")
    }

    // Ajax.get("http://api.openweathermap.org/" + "data/2.5/weather?q=Tampere").foreach {
    //     xhr =>
    //         println(xhr.responseText)
    // }

    import scala.concurrent._ // Future 때문에 추가
    import ExecutionContext.Implicits.global // 172번 줄 (onLoadFuture(img)) 에러 때문에 추가
    def onLoadFuture(img: HTMLImageElement) = {
        if (img.complete) {
            Future.successful(img.src)
        } else {
            val p = Promise[String]()
            img.onload = { (e: Event) => p.success(img.src)}
            p.future
        }
    }

    val img = dom.document.querySelector("#mapimage").asInstanceOf[HTMLImageElement]
    onLoadFuture(img).foreach { url => println(s"Image $url loaded") }

    implicit class HTMLImageElementOps(val img: HTMLImageElement) extends AnyVal {
        def onloadF = onLoadFuture(img)
    }

    val secImg = dom.document.querySelector("#mapimage").asInstanceOf[HTMLImageElement]
    secImg.onloadF.foreach { url => println(s"Image $url loaded")}
}
