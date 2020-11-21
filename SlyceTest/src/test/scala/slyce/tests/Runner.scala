package slyce.tests

import scala.io.Source

import scalaz.-\/
import scalaz.\/-

import slyce.parse.architecture.Parser

object Runner {

  def run[RawTree](parser: Parser[String, List[String], RawTree], srcFile: String)(f: RawTree => Unit): Unit = {
    val source = Source.fromFile(srcFile)
    val str = source.mkString
    source.close

    val res = parser(str)

    res match {
      case -\/(err) =>
        println("Error:")
        println()
        err.foreach(println)
      case \/-(rawTree) =>
        f(rawTree)
    }
  }

}
