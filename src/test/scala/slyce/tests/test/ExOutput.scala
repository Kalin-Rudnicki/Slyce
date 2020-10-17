package slyce.tests.test

import scalaz.Scalaz.ToOptionIdOps

import slyce.implementations.parsing._

object ExOutput {

  sealed trait Token

  object Token {

    final case class a(text: String) extends Token
    final case class b(text: String) extends Token
    final case class ab(text: String) extends Token

  }

  val dfa: Dfa[Token] = {
    lazy val s0: Dfa.State[Token] =
      Dfa.State(
        transitions = Map(
          'a' -> Lazy(s1).some
        ),
        elseTransition = None,
        yields = Dfa.State.Yield(s3)(Token.a(_).some).some
      )
    lazy val s1: Dfa.State[Token] =
      Dfa.State(
        transitions = Map(
          'a' -> Lazy(s1).some,
          'b' -> Lazy(s7).some
        ),
        elseTransition = None,
        yields = None
      )
    lazy val s2: Dfa.State[Token] =
      Dfa.State(
        transitions = Map(
          '/' -> Lazy(s6).some
        ),
        elseTransition = None,
        yields = None
      )
    lazy val s3: Dfa.State[Token] =
      Dfa.State(
        transitions = Map(
          'a' -> Lazy(s0).some,
          'b' -> Lazy(s4).some,
          '/' -> Lazy(s2).some
        ),
        elseTransition = None,
        yields = None
      )
    lazy val s4: Dfa.State[Token] =
      Dfa.State(
        transitions = Map(),
        elseTransition = None,
        yields = Dfa.State.Yield(s3)(Token.b(_).some).some
      )
    lazy val s5: Dfa.State[Token] =
      Dfa.State(
        transitions = Map(),
        elseTransition = None,
        yields = Dfa.State.Yield(s3)(_ => None).some
      )
    lazy val s6: Dfa.State[Token] =
      Dfa.State(
        transitions = Map(
          '\n' -> Lazy(s5).some
        ),
        elseTransition = Lazy(s6).some,
        yields = None
      )
    lazy val s7: Dfa.State[Token] =
      Dfa.State(
        transitions = Map(
          'b' -> Lazy(s8).some
        ),
        elseTransition = None,
        yields = None
      )
    lazy val s8: Dfa.State[Token] =
      Dfa.State(
        transitions = Map(
          'b' -> Lazy(s8).some
        ),
        elseTransition = None,
        yields = Dfa.State.Yield(s3)(Token.ab(_).some).some
      )

    Dfa(s3)
  }

}
