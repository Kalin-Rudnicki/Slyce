package slyce.lexer.nfa

import slyce.tree.GeneralToken

import scala.collection.mutable.{ListBuffer => MList, Map => MMap}

class TransitionMap[T <: GeneralToken] {
  
  val transitions: MMap[
    Char,
    MList[State[T]]
  ] = MMap()
  
  val epsilonTransitions: MList[State[T]] = MList()
  val unspecified: MList[State[T]] = MList()
  val unmatched: MList[State[T]] = MList()
  
  def <<(charClass: CharClass, transitionType: State[T]): Unit = charClass match {
    case CharClass.Unmatched() =>
      unmatched.append(transitionType)
    case CharClass.Only(chars) =>
      chars.foreach {
        c =>
          transitions
            .getOrElseUpdate(c, unspecified.clone)
            .append(transitionType)
      }
    case CharClass.Except(chars) =>
      transitions.foreach {
        case (k, v) =>
          if (!chars.contains(k))
            v.append(transitionType)
      }
      unspecified.append(transitionType)
  }
  
  def ~= (other: State[T]): Unit =
    epsilonTransitions.append(other)
  
}
