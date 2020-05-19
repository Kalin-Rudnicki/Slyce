package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}
import scala.collection.mutable.{Map => MMap}

import todo_move_tree.GeneralToken

class TransitionMap[T <: GeneralToken] {

  val transitions: MMap[
    Char,
    MList[State[T]]
  ] = MMap()

  val epsilonTransitions: MList[State[T]] = MList()
  // Unspecified has to do with not having to list out every UTF-8 char for [^a-z]
  val unspecified: MList[State[T]] = MList()

  def <<(charClass: CC, transitionType: State[T]): Unit =
    charClass match {
      case CC.Only(chars) =>
        chars.foreach { c =>
          transitions
            .getOrElseUpdate(c, unspecified.clone)
            .append(transitionType)
        }
      case CC.Except(chars) =>
        transitions.foreach {
          case (k, v) =>
            if (!chars.contains(k))
              v.append(transitionType)
        }
        unspecified.append(transitionType)
    }

  def ~=(other: State[T]): Unit =
    epsilonTransitions.append(other)

}
