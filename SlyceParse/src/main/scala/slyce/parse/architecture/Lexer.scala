package slyce.parse.architecture

import slyce.common.architecture.Stage

trait Lexer[Src, Errs, +Tok] extends Stage[Src, Errs, List[Tok]]
