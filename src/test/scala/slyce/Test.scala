package slyce

import slyce.lexer.Action._
import slyce.lexer.nfa.GenerationMessage
import slyce.lexer.nfa.RegexImplicits._
import slyce.lexer.nfa._
import slyce.tree.GeneralToken

object Test {

  trait Tok extends GeneralToken

  def main(args: Array[String]): Unit = {

    val nfa: NFA[Tok] = new NFA

    val m1: Mode[Tok] = nfa.initialMode

    (('c' :| 'C' <|> '_') * (3, None)).forEach(r => m1 |~> r |+ 4 >> "S")

    val msg = GenerationMessage.RepeatMinNegative(-3)
    println(msg)

  }

}
