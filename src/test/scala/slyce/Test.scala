package slyce

import scala.io.Source

object Test extends App {

  val chars: List[Char] = Source.fromFile("res-test/calc/src/calc.slf").toList
  
  println(Parser.parse(chars))
  
}
