package slyce.generate.architecture.lexer

import slyce.common.architecture.Stage
import slyce.common.helpers.Idt

trait DfaTokenLines[Dfa] extends Stage[Dfa, List[String], Idt]
