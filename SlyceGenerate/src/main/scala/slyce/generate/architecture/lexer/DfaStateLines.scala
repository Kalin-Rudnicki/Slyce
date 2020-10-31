package slyce.generate.architecture.lexer

import slyce.common.architecture.Stage
import slyce.common.helpers.Idt

trait DfaStateLines[Dfa] extends Stage[Dfa, List[String], Idt]
