package slyce.architecture.generation.lexer

import slyce.architecture.Stage

trait DataToNfa[Data, Err, Nfa] extends Stage[Data, Err, Nfa]
