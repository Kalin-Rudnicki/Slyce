package slyce.architecture.generation.lexer

import slyce.architecture.Stage

trait DataToNfa[Data, Nfa] extends Stage[Data, String, Nfa]
