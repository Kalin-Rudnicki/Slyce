package slyce.generate.architecture.lexer

import slyce.common.architecture.Stage

trait DataToNfa[Data, Err, Nfa] extends Stage[Data, Err, Nfa]
