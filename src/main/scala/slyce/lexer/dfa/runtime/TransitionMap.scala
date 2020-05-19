package slyce.lexer.dfa.runtime

import scalaz.std.option.optionSyntax._

import slyce.tree.GeneralToken

class TransitionMap[T <: GeneralToken](
    val transitions: Map[Char, State[T]],
    val defaultTransition: Option[State[T]]
) {

  def apply(char: Char): Option[State[T]] =
    transitions
      .get(char)
      .cata(
        _.some,
        defaultTransition
      )

}
