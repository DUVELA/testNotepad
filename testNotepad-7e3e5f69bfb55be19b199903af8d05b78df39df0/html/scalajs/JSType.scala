// package example

import org.scalajs.dom
import org.scalajs.dom.html
import org.querki.jquery._
import dom.document
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.Random
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js
import scala.collection.mutable
import io.udash.wrappers.jquery._

object Hello extends Greeting with App {
  val scalaFun: Int => Int = (x: Int) => x * x
  val jsFun: js.Function1[Int, Int] = scalaFun
  val scalaFunAgain: Int => Int = jsFun
  println(greeting)
//js.Array[T] <–> mutable.Seq[T]
  val jsArr = js.Array(1, 2, 3)
  val xs: js.Array[Int] = jsArr.takeWhile(_ < 3)
  val ys: mutable.Seq[Int] = jsArr
  val zs: scala.Array[Int] = jsArr.toArray
  
  import js.JSConverters._
  val scSeq = Seq(1, 2, 3)
  val jsArray: js.Array[Int] = scSeq.toJSArray

//js.Dictionary[T] <–> mutable.Map[String, T]
  import scala.scalajs.js
  val jsDict = js.Dictionary("a" -> 1, "b" -> 2)
  val xm = jsDict.mapValues(_ * 2)
  // val xm: mutable.Map[String, Int] = jsDict.mapValues(_ * 2)
  val ym: mutable.Map[String, Int] = jsDict
  // import js.JSConverters._
  val scMap = Map("a" -> 1, "b" -> 2)
  val jsDictionary: js.Dictionary[Int] = scMap.toJSDictionary
//js.UndefOr[T] <-> Option[T]
  //import scala.scalajs.js
  val jsUndefOr: js.UndefOr[Int] = 1
  val xo: Option[Int] = jsUndefOr.toOption
  //import js.JSConverters._
  val opt = Some(1)
  val yu: js.UndefOr[Int] = opt.orUndefined
  val fj: js.Function1[Double, Double] = { (x: Double) => x * x}
//   var f = function(x) {
//      return x*x;
//   };
  val yf = fj(5)
  /*
    var lis = jQuery("ol > li");
    lis.each(function() {
      jQuery(this).text(jQuery(this).text() + " - transformed")
    });
  */
  // jQ("#elementId")
  // .hide(AnimationOptions(
  //   duration = Some(3000),
  //   easing = Some(EasingFunction.linear)
  // ))
  // .show(1500, EasingFunction.swing)

  // val lis = jQuery("ol > li")
  // lis.each({ (li: dom.HTMLElement) =>
  //   jQuery(li).text(jQuery(li).text() + " - transformed")
  // }: js.ThisFunction)

  val fjeeo: js.ThisFunction1[js.Object, Int, Int] = ???
  val o = new js.Object
  val xfojeeo = fjeeo(o, 4)

  /*
  var f = ...;
  var o = new Object();
  var x = f.call(o, 4);
  */
  val jdocument = js.Dynamic.global.document
  val playground = jdocument.getElmentById("playground")

  val newP = jdocument.createElement("p")
  newP.innerHTML = "Hello world! <i>-- DOM</i>"
  playground.appendChild(newP)
}

trait MyObject extends js.Object {
  val foo: Int = js.native
  val bar: String = js.native
}

object MyObject {
  def apply(foo: Int, bar: String): MyObject = js.Dynamic.literal(foo = foo, bar = bar).asInstanceOf[MyObject]
}

trait Greeting {
  lazy val greeting: String = "hello html"
}
