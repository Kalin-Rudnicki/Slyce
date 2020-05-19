package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}

import klib.handling.MessageAccumulator
import slyce.generation.GenerationMessage
import slyce.generation.generated.lexer.dfa.DFA

class NFA(initialModeName: String = "General") {

  val initialMode: Mode = new Mode(initialModeName)
  val modes: MList[Mode] = MList(initialMode)

  def newMode(name: String): Mode = {
    val mode: Mode = new Mode(name)
    modes.append(mode)
    mode
  }

  /**
    * @return (Compiled DFA, List of (Unused line, Lines that override it))
    */
  def compile: MessageAccumulator[GenerationMessage, DFA] = { // TODO (KR) : Reference correct DFA
    // TODO (KR) : Implement
    ???
  }

}
