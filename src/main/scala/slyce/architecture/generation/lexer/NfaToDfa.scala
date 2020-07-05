package slyce.architecture.generation.lexer

import slyce.architecture.Stage

trait NfaToDfa[Nfa, Dfa] extends Stage[Nfa, String, Dfa]
