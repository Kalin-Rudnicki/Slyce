package slyce.generate.lexer

import slyce.common.helpers

import Yields.Yield

final case class Dfa(initialState: Dfa.State) {

  val idxOf: Map[Dfa.State, Int] = {
    val tmp = initialState.findAll.toList.zipWithIndex.toMap

    tmp.map {
      case (s @ this.initialState, _) =>
        s -> 0
      case (s, 0) =>
        s -> tmp(initialState)
      case (s, i) =>
        s -> i
    }
  }

}

object Dfa {

  final class State {

    var transitions: Map[Set[Char], Option[State]] = _
    var elseTransition: Option[State] = _
    var yields: Option[State.Yields] = None

    def findAll: Set[State] =
      State.findAll(Set(this))

  }

  object State {

    final case class Yields(
        yields: List[Yield],
        toMode: State,
    )

    def findAll(unseen: Set[State], seen: Set[State] = Set()): Set[State] =
      helpers.findAll(unseen, seen) { s =>
        s.transitions.toList.flatMap(_._2.toList).toSet |
          s.elseTransition.toSet |
          s.yields.map(_.toMode).toSet
      }

  }

}
