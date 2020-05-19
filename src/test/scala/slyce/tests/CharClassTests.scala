package slyce.tests

import scala.collection.mutable.{Map => MMap}

import org.scalatest.Matchers._
import org.scalatest.funspec.PathAnyFunSpec
import slyce.generation.raw.lexer.nfa.Regex.{CharClass => CC}
import slyce.generation.raw.lexer.nfa.RegexImplicits._

class CharClassTests extends PathAnyFunSpec {

  private val o0: CC = CC.only()

  private val o_AB: CC = CC.only("AB")
  private val o_BC: CC = CC.only("BC")

  private val e_AB: CC = CC.except("AB")
  private val e_BC: CC = CC.except("BC")

  val testMap: MMap[String, MMap[String, MMap[Char, Int]]] = MMap()

  val test: String => String => (Char, => CC, => CC) => Unit =
    outer =>
      inner =>
        (op, exp, act) => {
          val myMap: MMap[Char, Int] =
            testMap
              .getOrElseUpdate(outer, MMap())
              .getOrElseUpdate(inner, MMap())
          val testNo: Int = myMap.getOrElse(op, 1)
          myMap.put(op, testNo + 1)

          it(s"Test '$op' $testNo") {
            act shouldEqual exp
          }
        }

  implicit def toHelper1(cc: => CC): Helper1 =
    new Helper1(cc)

  class Helper1(cc: => CC) {

    def ::|(that: => CC): Helper2 =
      new Helper2('|', cc :| that)

    def ::&(that: => CC): Helper2 =
      new Helper2('&', cc :& that)

    def ::-(that: => CC): Helper2 =
      new Helper2('-', cc :- that)

  }

  class Helper2(op: Char, cc: => CC) {

    def ===(that: => CC)(implicit whereAmI: (String, String)): Unit =
      test(whereAmI._1)(whereAmI._2)(op, that, cc)

  }

  describe("left : Only") {

    describe("right : Only") {
      implicit val whereIAm: (String, String) = ("Only", "Only")

      // =====| "|" |=====

      o0 ::| o0 === o0
      o0 ::| o_BC === o_BC

      o_AB ::| o0 === o_AB

      o_AB ::| o_AB === o_AB

      o_AB ::| o_BC === CC.only("ABC")

      // =====| "&" |=====

      o0 ::& o0 === o0
      o0 ::& o_AB === o0

      o_AB ::& o0 === o0

      o_AB ::& o0 === o0
      o_AB ::& o_BC === CC.only("B")

      // =====| "-" |=====

      o0 ::- o0 === o0
      o0 ::- o_AB === o0

      o_AB ::- o0 === o_AB

      o_AB ::- o_BC === CC.only("A")

    }

    describe("right : Except") {
      implicit val whereIAm: (String, String) = ("Only", "Except")

      // =====| "|" |=====

      o0 ::| e_BC === e_BC

      o_AB ::| e_BC === CC.except("C")

      // =====| "&" |=====

      o0 ::& e_BC === o0

      o_AB ::& e_BC === CC.only("A")

      // =====| "-" |=====

      o0 ::- e_BC === o0

      o_AB ::- e_BC === CC.only("B")

    }

  }

  describe("left : Except") {

    describe("right : Only") {
      implicit val whereIAm: (String, String) = ("Except", "Only")

      // =====| "|" |=====

      e_AB ::| o0 === e_AB

      e_AB ::| o_BC === CC.except("A")

      // =====| "&" |=====

      e_AB ::& o0 === o0

      e_AB ::& o_BC === CC.only("C")

      // =====| "-" |=====

      e_AB ::- o0 === e_AB

      e_AB ::- o_BC === CC.except("ABC")

    }

    describe("right : Except") {
      implicit val whereIAm: (String, String) = ("Except", "Except")

      // =====| "|" |=====

      e_AB ::| e_BC === CC.except("B")

      // =====| "&" |=====

      e_AB ::& e_BC === CC.except("ABC")

      // =====| "-" |=====

      e_AB ::- e_BC === CC.only("C")

    }

  }

}
