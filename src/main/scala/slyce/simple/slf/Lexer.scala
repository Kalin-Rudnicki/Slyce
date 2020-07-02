package slyce.simple.slf

import scala.annotation.tailrec

import slyce.architecture.Stage
import slyce.simple.Ref
import slyce.simple.Tokenizer

object Lexer {

  val stage: Stage[List[Char], String, List[Token]] = {
    val t0: Ref[Tokenizer[Token]] = Ref.empty

    t0.set(Tokenizer().withNlWs)

    t0.value.tokenize
  }

  sealed trait Token
  object Token {
    // TODO (KR) :
  }

}
