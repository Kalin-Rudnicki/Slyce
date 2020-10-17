package slyce.implementations.generation.lexer

import Yields.Yield

final case class Dfa(initialState: Dfa.State)

object Dfa {

  final class State {

    var transitions: Map[Set[Char], Option[State]] = _
    var elseTransition: Option[State] = _
    var yields: Option[State.Yields] = None

  }

  object State {

    final case class Yields(
        yields: Option[Yield],
        toMode: State
    )

  }

}
