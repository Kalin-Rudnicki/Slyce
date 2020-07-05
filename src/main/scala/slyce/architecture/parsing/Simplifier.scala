package slyce.architecture.parsing

import slyce.architecture.Stage

trait Simplifier[RawTree, Errs, SimpleTree] extends Stage[RawTree, Errs, SimpleTree]
