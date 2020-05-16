package slyce

import slyce.lexer.nfa._
import slyce.tree.GeneralToken
import slyce.lexer.Action._
import slyce.lexer.nfa.RegexImplicits._

object Test {

  trait Tok extends GeneralToken

  def main(args: Array[String]): Unit = {

    val nfa: NFA[Tok] = new NFA

    val m1: Mode[Tok] = nfa.initialMode

    m1 |~> 'c' :| 'C' <|> '_' |+ 4 >> "S"

  }

}
