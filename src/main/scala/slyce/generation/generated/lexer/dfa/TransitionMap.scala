package slyce.generation.generated.lexer.dfa

import scala.language.implicitConversions

import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.ToOptionOpsFromOption

class TransitionMap(transitions: Map[Char, State], default: Option[State]) {

  def apply(char: Char): Option[State] =
    transitions
      .get(char)
      .cata(
        _.some,
        default
      )

}
