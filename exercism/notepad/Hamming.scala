import scala.annotation.tailrec
object Hamming {
    val mutantTest = "[^ACGT]".r

    def distance(az: String, za: String) = {
        val look = az.zip(za)
                
        @tailrec
        def show(look: Seq[(Char, Char)])(acc: Int): Option[Int] = if (az.equals(mutantTest.replaceAllIn(az,"")) && za.equals(mutantTest.replaceAllIn(za,"")) && az.size == za.size) look match {
            case x if (look.isEmpty) => Option(acc)
            case x if (az == za) => Some(0)
            case x if (look.head._1 == look.head._2) => show(look.tail)(acc)
            case x if (look.head._1 != look.head._2) => show(look.tail)(acc + 1)
        }
        else None
        show(look)(0)
    }
}


/*
import scala.annotation.tailrec
object Hamming {
    val mutantTest = "[^ACGT]".r

    def distance(az: String, za: String) = if (az.equals(mutantTest.replaceAllIn(az,"")) && za.equals(mutantTest.replaceAllIn(za,""))) az match {
        case x if az.size != za.size => None
        case x if (az == za) => Some(0)
        case _ => {

            val look = az.zip(za)
            @tailrec
            def show(look: Seq[(Char, Char)])(acc: Int): Option[Int] = look match {
                case x if (look.isEmpty) => Option(acc)
                case x if (look.head._1 == look.head._2) => show(look.tail)(acc)
                case x if (look.head._1 != look.head._2) => show(look.tail)(acc + 1)
                case _ => Option(acc)
            }
            show(look)(0)
        }
    }
    else None
}

// val azList = az.toList // List[Char]
// def b1Distance(azList: List[Char], acc: Int)(zaList: List[Char]): Option[Int] = azList match {
//                 case a if (a.isEmpty) || (zaList.isEmpty) => Option(acc)
//                 case a if (a != zaList.head) => b1Distance(azList.tail, acc + 1)(zaList.tail.drop(acc))
//                 case a if (a == zaList.head) => b1Distance(azList.tail, acc)(zaList.tail.drop(acc))
//             }
//             b1Distance(azList, 0)(zaList)
//             (Some(2) Hamming.distance("AT", "CT") should be (Some(1))
*/
