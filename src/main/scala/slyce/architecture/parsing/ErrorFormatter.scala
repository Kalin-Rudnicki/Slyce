package slyce.architecture.parsing

import slyce.architecture.Stage

trait ErrorFormatter[Src, Errs] extends Stage[(Src, Errs), Nothing, String]
