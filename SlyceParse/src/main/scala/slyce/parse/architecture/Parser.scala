package slyce.parse.architecture

import slyce.common.architecture.Stage

trait Parser[Src, Errs, RawTree] extends Stage[Src, Errs, RawTree]
object Parser {

  def apply[Errs, Src, Tok, RawTree](
      lexer: Lexer[Src, Errs, Tok],
      grammar: Grammar[Tok, Errs, RawTree],
  ): Parser[Src, Errs, RawTree] =
    (lexer.onO(_.foreach(println)) >+> grammar)(_)

}
