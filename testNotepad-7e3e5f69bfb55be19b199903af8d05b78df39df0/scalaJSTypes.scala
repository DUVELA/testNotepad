/*
http://www.scala-js.org/doc/interoperability/types.html
js.Dictionary[T] <–> mutable.Map[String, T]
=> https://gitter.im/scala-js/scala-js/archives/2018/09/22
*/
package example

import scala.scalajs.js
import org.querki.jquery._
import js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel
// import org.scalajs.jquery.JQueryStatic
import scala.scalajs.js.annotation.JSImport
import org.scalajs.dom
import org.scalajs.dom.html
import dom.document
import org.scalajs.jquery.jQuery
import org.scalajs.dom.html
import org.scalajs.dom.raw.{ Element, HTMLElement }

object Hello extends Greeting with App {
  println(greeting)
  val scalaFun: Int => Int = (x: Int) => x * x
  val jsFun: js.Function1[Int, Int] = scalaFun
  val scalaFunAgain: Int => Int = jsFun


  val jsDict = js.Dictionary("a" -> 1, "b" -> 2)
  val x: collection.Map[String, Int] = jsDict.mapValues(_ * 2) // val x: mutable.Map[String, Int] = jsDict.mapValues(_ * 2)
  val y: collection.Map[String, Int] = jsDict                  // val y: mutable.Map[String, Int] = jsDict
  val scMap = Map("a" -> 1, "b" -> 2)
  
//   val lis = jQuery("ol > li")          // import import org.scalajs.jquery.jQuery,
//   lis.each({ (li: dom.HTMLElement) =>
//     jQuery(li).text(jQuery(li).text() + " - transformed")
//   }: js.ThisFunction)
  
}

trait Greeting {
  lazy val greeting: String = "hello"
}
