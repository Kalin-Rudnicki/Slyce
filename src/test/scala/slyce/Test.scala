package slyce

import slyce.lexer.nfa.{Regex => R}
import R.{CharClass => CC}
import CC.{Common => CCC}

object Test {
  
  def main(args: Array[String]): Unit = {
    
    val r1 = CCC.lowerLetters >> (CCC.lowerLetters | CCC.upperLetters)
    
    println(r1.prettyStr)
    
  }
  
}