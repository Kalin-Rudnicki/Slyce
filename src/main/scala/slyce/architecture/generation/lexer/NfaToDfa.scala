package slyce.architecture.generation.lexer

import slyce.architecture.Stage

trait NfaToDfa[Nfa, Err, Dfa] extends Stage[Nfa, Err, Dfa]
