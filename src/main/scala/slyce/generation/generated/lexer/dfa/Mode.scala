package slyce.generation.generated.lexer.dfa

class Mode(val name: String) {

  var states: List[State] = _

  def init(s: List[State]): Unit =
    states = s

}
