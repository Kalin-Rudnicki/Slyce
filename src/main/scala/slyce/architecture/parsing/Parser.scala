package slyce.architecture.parsing

import slyce.architecture.Stage

trait Parser[Src, Errs, SimpleTree] extends Stage[Src, Errs, SimpleTree]
object Parser {

  def apply[Errs, Src, Tok, RawTree, SimpleTree, Formatter](
      lexer: Lexer[Src, Errs, Tok],
      grammar: Grammar[Tok, Errs, RawTree],
      simplifier: Simplifier[RawTree, Errs, SimpleTree],
      formatter: ErrorFormatter[Src, Errs]
  ): Parser[Src, Errs, SimpleTree] =
    (lexer >+> grammar >+> simplifier)(_)

}
