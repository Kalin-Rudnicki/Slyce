package slyce.generation.generated.lexer.dfa

import slyce.generation.TokenSpec

case class Action(lineNo: Int, tokenSpecs: List[TokenSpec], modeStart: State)
