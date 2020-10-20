package slyce.parse.architecture

import slyce.common.architecture.Stage

trait Grammar[Tok, Errs, RawTree] extends Stage[List[Tok], Errs, RawTree]
