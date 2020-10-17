package slyce.implementations.parsing

final case class Dfa[+Tok](initialState: Dfa.State[Tok])

object Dfa {

  final case class State[+Tok](
      transitions: Map[Char, Option[Lazy[State[Tok]]]],
      elseTransition: Option[Lazy[State[Tok]]],
      yields: Option[State.Yield[Tok]]
  ) {

    def apply(c: Char): Option[State[Tok]] =
      transitions.getOrElse(c, elseTransition).map(_.value)

  }

  object State {

    final class Yield[+Tok](_to: => State[Tok], val yields: String => Option[Tok]) {
      lazy val to: State[Tok] = _to
    }

    object Yield {

      def apply[Tok](to: State[Tok])(yields: String => Option[Tok]): Yield[Tok] =
        new Yield(to, yields)

    }

  }

}
