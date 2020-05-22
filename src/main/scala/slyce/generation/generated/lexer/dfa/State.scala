package slyce.generation.generated.lexer.dfa

class State(val modeName: String, val id: Int) {

  var transitions: TransitionMap = _
  var action: Option[Action] = _

  def init(tm: TransitionMap): Unit =
    transitions = tm

  def apply(char: Char): Option[State] =
    transitions(char)

}
