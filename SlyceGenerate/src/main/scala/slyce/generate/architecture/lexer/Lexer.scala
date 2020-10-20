package slyce.generate.architecture.lexer

import slyce.common.architecture.Stage

trait Lexer[Data, Err, Dfa] extends Stage[Data, Err, Dfa]

object Lexer {

  def apply[Data, Err, Nfa, Dfa](
      dataToNfa: DataToNfa[Data, Err, Nfa],
      nfaToDfa: NfaToDfa[Nfa, Err, Dfa],
  ): Lexer[Data, Err, Dfa] =
    (dataToNfa >+> nfaToDfa)(_)

}
