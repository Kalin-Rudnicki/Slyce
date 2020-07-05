package slyce.architecture.parsing

import slyce.architecture.Stage

trait Grammar[Tok, Errs, RawTree] extends Stage[List[Tok], Errs, RawTree]
