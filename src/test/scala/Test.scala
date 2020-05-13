import slyce.tree.GeneralToken
import slyce.tree.GeneralToken.Stats
import slyce.lexer.nfa.NFA

import scala.io.Source

object Test {
  
  trait MyTokType extends GeneralToken
  
  object MyTokType {
    
    case class MyHello(stats: Stats) extends MyTokType
    
    case class MyHi(stats: Stats) extends MyTokType
    
    case class MyHola(stats: Stats) extends MyTokType
    
  }
  
  import MyTokType._
  
  def main(args: Array[String]): Unit = {
    
    val nfa: NFA[MyTokType] = new NFA
    
    val m0 = nfa.initialMode;
    {
      val s0 = m0.initialState
      val s1 = m0.newState
      val s2 = m0.newState
      val s3 = m0.newState
      val s4 = m0.newState
      val s5 = m0.newState
      val s6 = m0.newState
      val s7 = m0.newState
      val s8 = m0.newState
      val s9 = m0.newState
      val s10 = m0.newState
      val s11 = m0.newState
      val s12 = m0.newState
      val s13 = m0.newState
      val s14 = m0.newState
      
      s0 <+> s1
      s1 + List('H') >> s2
      s2 + List('e') >> s3
      s3 + List('l') >> s4
      s4 + List('l') >> s5
      s5 + List('o') >> s6
      s6 @@ 1 >> { s => List(MyHello(s)) }
      
      s0 <+> s7
      s7 + List('H') >> s8
      s8 + List('i') >> s9
      s9 @@ 2 >> { s => List(MyHi(s)) }
      
      s7 + List('H') >> s10
      s10 + List('o') >> s11
      s11 + List('l') >> s12
      s12 + List('a') >> s13
      s13 @@ 3 >> { s => List(MyHola(s)) }
      
      s0 + List(' ', '\n') >> s14
      s14 + List(' ', '\n') >> s14
      s14 @@ 4 >> { _ => Nil }
      
      test
      
    }
    
  }
  
  def test: Unit = {
    
    val src = Source.fromFile("res/test.txt")
    
    src.getLines().foreach {
      l => println(s"|$l|")
    }
    
    src.close
    
  }
  
}