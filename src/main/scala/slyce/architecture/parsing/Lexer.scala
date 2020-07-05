package slyce.architecture.parsing

import slyce.architecture.Stage

trait Lexer[Src, Errs, Tok] extends Stage[Src, Errs, List[Tok]]
