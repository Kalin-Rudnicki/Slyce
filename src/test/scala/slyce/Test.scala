package slyce

import slyce.generation.GenerationMessage
import todo_remove_lexer.Action._

import slyce.generation.raw.lexer.nfa.RegexImplicits._
import todo_remove_lexer.nfa._
import todo_move_tree.GeneralToken

import slyce.generation.raw.lexer.nfa.Mode
import slyce.generation.raw.lexer.nfa.NFA

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
