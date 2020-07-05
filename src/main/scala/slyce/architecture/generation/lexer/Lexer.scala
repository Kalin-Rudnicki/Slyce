package slyce.architecture.generation.lexer

import slyce.architecture.Stage

trait Lexer[Data, Dfa] extends Stage[Data, String, Dfa]
object Lexer {

  def apply[Data, Nfa, Dfa](
      dataToNfa: DataToNfa[Data, Nfa],
      nfaToDfa: NfaToDfa[Nfa, Dfa]
  ): Lexer[Data, Dfa] =
    (dataToNfa >+> nfaToDfa)(_)

}
