package slyce.parse.architecture

import slyce.common.architecture.Stage

trait Simplifier[RawTree, Errs, SimpleTree] extends Stage[RawTree, Errs, SimpleTree]
