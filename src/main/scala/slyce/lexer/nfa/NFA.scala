package slyce.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}

import slyce.lexer.dfa.runtime.DFA
import slyce.tree.GeneralToken

class NFA[T <: GeneralToken](initialModeName: String = "General") {

  val initialMode: Mode[T] = new Mode[T](initialModeName)
  val modes: MList[Mode[T]] = MList(initialMode)

  def newMode(name: String): Mode[T] = {
    val mode: Mode[T] = new Mode[T](name)
    modes.append(mode)
    mode
  }

  /**
    * @return (Compiled DFA, List of (Unused line, Lines that override it))
    */
  def compile: (Option[DFA[T]], List[GenerationMessage]) = {
    // TODO (KR) : Implement
    ???
  }

}
