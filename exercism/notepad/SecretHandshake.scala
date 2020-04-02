object SecretHandshake {

    val handshake = List(('0',"jump"), ('0',"close your eyes"), ('0',"double blink"), ('0',"wink"))

    def commands(ddt: Long) = {
        if(32 >= ddt && ddt >= 0) {
            val tBinaryList = (63 ^ ddt).toBinaryString.toList // 3.toBinaryString = 11; (31 ^ 31).toBinaryString = 0
            val duv = for(x <- 0 to tBinaryList.splitAt(2)._2.size -1) yield ((tBinaryList.splitAt(2)._2(x), handshake(x)._1), handshake(x)._2)
            
            ddt match {
                case x if(32 >= x && x >= 16) => duv.filter((t) => t._1 == ('0', '0')).map{case ((x,y),z) => z}.toList
                case _ => duv.filter((t) => t._1 == ('0', '0')).map{case ((x,y),z) => z}.toList.reverse
            }
        }
        else s"found :$ddt \nrequired: 0 ~ 32"
    }
}

/* 
    scala> commands(64) java.lang.IndexOutOfBoundsException
    scala> commands(-1) java.lang.IndexOutOfBoundsException
*/
/* 
    scala> duv.filter((t) => t._1 != ('1', '0')).map{case ((x,y),z) => z.toString}.toList
    res7: List[String] = List(double blink, wink)

    scala> duv.map{case ((x,y),z) if(x == y) => z}.toList
    scala.MatchError: ((1,0),jump) (of class scala.Tuple2)
*/


/*
    scala> val ddt = for(x <- 0 to (63 ^ 19).toBinaryString.splitAt(2)._2.toList.size - 1) yield ((63 ^ 19).toBinaryString.splitAt(2)._2(x),sample(x))
    ddt: scala.collection.immutable.IndexedSeq[(Char, (Char, String))] = Vector((1,(0,jump)), (1,(0,close your eyes)), (0,(0,double blink)), (0,(0,wink)))
*/

/* rmdir /s /q
 def dTest(ddt: List[Int])(acc: List[Int] = List()): List[Int] = ddt match {
      case x :: xs => dTest(xs)(acc:+ (x + 1))
      case _ => acc
      }

*/

/*
 val ddt = 19.toBinaryString.toList // List(1, 0, 0, 1, 1)
 val dtt = 3.toBinaryString.toList // List(1, 1)
*/
