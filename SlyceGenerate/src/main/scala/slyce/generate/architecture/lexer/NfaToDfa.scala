package slyce.generate.architecture.lexer

import slyce.common.architecture.Stage

trait NfaToDfa[Nfa, Err, Dfa] extends Stage[Nfa, Err, Dfa]
