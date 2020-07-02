package slyce.tests.simple.slf

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import scalaz.-\/
import scalaz.\/-

import slyce.simple.slf.Lexer.stage

class SlfLexerTests extends AnyFunSpec {

  describe("Simple - Slf - Lexer") {

    it("is empty on nothing") {
      stage(Nil) match {
        case -\/(msg) =>
          fail(msg)
        case \/-(tokens) =>
          tokens.shouldEqual(Nil)
      }
    }

    it("is empty on whitespace/newlines") {
      stage(" \t \n \t \n".toList) match {
        case -\/(msg) =>
          fail(msg)
        case \/-(tokens) =>
          tokens.shouldEqual(Nil)
      }
    }

  }

}
