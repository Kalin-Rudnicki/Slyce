package slyce.parse.architecture

import slyce.common.architecture.Stage

trait ErrorFormatter[Src, Errs] extends Stage[(Src, Errs), Nothing, String]
