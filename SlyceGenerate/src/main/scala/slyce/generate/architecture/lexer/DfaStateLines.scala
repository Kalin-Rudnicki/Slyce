package slyce.generate.architecture.lexer

import slyce.common.architecture.Stage

trait DfaStateLines[Dfa] extends Stage[(Dfa, String), List[String], List[String]]
