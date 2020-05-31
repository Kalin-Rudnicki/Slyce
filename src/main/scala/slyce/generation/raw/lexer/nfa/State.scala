package slyce.generation.raw.lexer.nfa

import scala.language.implicitConversions

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer => MList}

import scalaz.Scalaz.ToBooleanOpsFromBoolean

import klib.fp.instances.{given _}
import klib.fp.ops.{given _}
import klib.handling.MessageAccumulator.Alive
import slyce.generation.GenerationMessage._
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

  def trivial_? : Boolean =
    actions.toList.isEmpty && transitions.transitions.isEmpty && transitions.unspecified.isEmpty

  // Epsilon transition
  def |==(other: State): Unit =
    transitions ~= other

  def <<<(charClass: CC): State = {
    val ns: State = mode.newState
    transitions << (charClass, ns)
    ns
  }

  // Transition on regex
  def |~>(regex: Regex): ??[State] =
    regex match {
      case Group(options) =>
        val newState: State = mode.newState
        for {
          opts <- options.list.toList.map(this |~> _).invert
          _ = opts.foreach(_ |== newState)
        } yield newState
      case Sequence(list) =>
        // NOTE (KR) : I think this is right, as it is in reverse order?
        list.foldRight(
          Alive(this).asInstanceOf[??[State]]
        ) { (reg, s) =>
          s.flatMap(_ |~> reg)
        }
      case cc: CC =>
        (this <<< cc)._lift[??]
      case repeat: Repeat =>
        @tailrec
        def rec(input: ??[State], times: Int, reg: Regex): ??[State] =
          if (times > 0)
            rec(
              input.flatMap(_ |~> regex),
              times - 1,
              reg
            )
          else
            input

        @tailrec
        def loop(input: ??[State], times: Int, reg: Regex, skip: State = mode.newState): ??[State] = {
          input.forEach(_ |== skip)
          if (times > 0)
            loop(
              input.flatMap(_ |~> reg),
              times - 1,
              reg,
              skip
            )
          else
            skip.lift[??]
        }

        repeat match {
          case Repeat.Between(min, max, reg) =>
            if (min < 0)
              ???
            else if (max <= 0)
              ???
            else
              loop(
                rec(this.lift[??], min, reg),
                max - min,
                reg
              )
          case Repeat.Infinite(min, reg) =>
            if (min < 0)
              ???
            else
              for {
                afterMin <- rec(this.lift[??], min, reg)
                take <- afterMin |~> reg
                _ = take |== afterMin
              } yield afterMin.free
        }
    }

  // Add action
  def |+(action: Action): Unit =
    actions.append(action)

}

object State {

  def epsilons(states: Set[State], stateToStateSet: State => Set[State]): Set[State] = {
    def loop(seen: Set[State], _new: Set[State]): Set[State] =
      _new.isEmpty.fold(
        seen,
        _new.foldLeft(seen & _new) { (__seen, s) =>
          loop(__seen, __seen &~ stateToStateSet(s))
        }
      )

    loop(Set(), states)
  }

}
