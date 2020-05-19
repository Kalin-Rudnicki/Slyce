package slyce.generation.generated.lexer.dfa

class State(val id: Int, val action: Option[Action]) {

  var transitions: TransitionMap = _

  def init(tm: TransitionMap): Unit =
    transitions = tm

  def apply(char: Char): Option[State] =
    transitions(char)

}
