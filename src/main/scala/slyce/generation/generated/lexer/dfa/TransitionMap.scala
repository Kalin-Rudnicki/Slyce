package slyce.generation.generated.lexer.dfa

import scalaz.std.option.optionSyntax._

class TransitionMap(transitions: Map[Char, State], default: Option[State]) {

  def apply(char: Char): Option[State] =
    transitions
      .get(char)
      .cata(
        _.some,
        default
      )

}
