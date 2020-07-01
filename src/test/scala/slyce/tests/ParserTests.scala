package slyce.tests

import org.scalactic.source.Position
import org.scalatest._
import scalaz.-\/
import scalaz.\/
import scalaz.\/-

import slyce.Parser

class ParserTests extends FunSpec {

  describe("include parser") {
    def shouldPass(input: List[Char])(remaining: List[Char], strings: List[String])(implicit position: Position): Unit = {
      val res: String \/ (List[Char], List[String]) = Parser.parseIncludes(input)
      res match {
        case -\/(_) =>
          fail
        case \/-((rem, strs)) =>
          assert(rem == remaining)
          assert(strs == strings)
      }
    }

    it("fully accepts newlines") {
      shouldPass("".toList)(Nil, Nil)
      shouldPass("\n".toList)(Nil, Nil)
      shouldPass("\n\n".toList)(Nil, Nil)
      shouldPass("\n\n\n".toList)(Nil, Nil)
    }

    it("accepts newlines until invalid") {
      shouldPass("\na".toList)('a' :: Nil, Nil)
      shouldPass("\n\na".toList)('a' :: 'b' :: Nil, Nil)
    }

  }

}
