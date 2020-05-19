package slyce.generation.raw.lexer.nfa

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer => MList}

import slyce.generation.raw.lexer.nfa.Regex._
import slyce.generation.raw.lexer.nfa.Regex.{CharClass => CC}

class State(val mode: Mode, val id: Int) {

  val transitions: TransitionMap = new TransitionMap()
  val actions: MList[Action] = MList()

  def free: State = {
    val newState = mode.newState
    this |== newState
    newState
  }

  // Epsilon transition
  def |==(other: State): Unit =
    transitions ~= other

  def <<<(charClass: CC): State = {
    val ns: State = mode.newState
    transitions << (charClass, ns)
    ns
  }

  // Transition on regex
  def |~>(regex: Regex): State =
    regex match {
      case Group(options) =>
        val newState: State = mode.newState
        options.map(this |~> _).foreach(_ |== newState)
        newState
      case Sequence(list) =>
        // NOTE (KR) : I think this is right, as it is in reverse order?
        list.foldRight(this)((reg, s) => s |~> reg)
      case cc: CC =>
        this <<< cc
      case repeat: Repeat =>
        @tailrec
        def rec(input: State, times: Int, reg: Regex): State =
          if (times > 0)
            rec(
              input |~> regex,
              times - 1,
              reg
            )
          else
            input

        @tailrec
        def loop(input: State, times: Int, reg: Regex, skip: State = mode.newState): State = {
          input |== skip
          if (times > 0)
            loop(
              input |~> reg,
              times - 1,
              reg,
              skip
            )
          else
            skip
        }

        repeat match {
          case Repeat.Between(min, max, reg) =>
            loop(
              rec(this, min, reg),
              max - min,
              reg
            )
          case Repeat.Infinite(min, reg) =>
            val afterMin: State = rec(this, min, reg)
            val take: State = afterMin |~> reg
            take |== afterMin
            afterMin.free
        }
    }

  // Add action
  def |+(action: Action): Unit =
    actions.append(action)

}
