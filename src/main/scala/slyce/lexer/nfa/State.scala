package slyce.lexer.nfa

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer => MList}

import slyce.lexer.Action
import slyce.lexer.nfa.Regex._
import slyce.lexer.nfa.Regex.{CharClass => CC}
import slyce.tree.GeneralToken

class State[T <: GeneralToken](val mode: Mode[T], val id: Int) {

  val transitions: TransitionMap[T] = new TransitionMap()
  val actions: MList[Action] = MList()

  def free: State[T] = {
    val newState = mode.newState
    this |== newState
    newState
  }

  // Epsilon transition
  def |==(other: State[T]): Unit =
    transitions ~= other

  def <<<(charClass: CC): State[T] = {
    val ns: State[T] = mode.newState
    transitions << (charClass, ns)
    ns
  }

  // Transition on regex
  def |~>(regex: Regex): State[T] =
    regex match {
      case Group(options) =>
        val newState: State[T] = mode.newState
        options.map(this |~> _).foreach(_ |== newState)
        newState
      case Sequence(list) =>
        // TODO (KR) : I think this is right, as it is in reverse order?
        list.list.foldRight(this)((reg, s) => s |~> reg)
      case cc: CC =>
        this <<< cc
      case repeat: Repeat =>
        @tailrec
        def rec(input: State[T], times: Int, reg: Regex): State[T] =
          if (times > 0)
            rec(
              input |~> regex,
              times - 1,
              reg
            )
          else
            input

        @tailrec
        def loop(input: State[T], times: Int, reg: Regex, skip: State[T] = mode.newState): State[T] = {
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
            val afterMin: State[T] = rec(this, min, reg)
            val take: State[T] = afterMin |~> reg
            take |== afterMin
            afterMin.free
        }
    }

  // Add action
  def |+(action: Action): Unit =
    actions.append(action)

}
