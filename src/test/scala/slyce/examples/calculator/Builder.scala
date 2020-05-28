package slyce.examples.calculator

import klib.fp.instances._
import klib.fp.ops._
import slyce.generation.GenerationMessage.??
import slyce.generation.TokenSpec._
import slyce.generation.raw.lexer.nfa.Action._
import slyce.generation.raw.lexer.nfa.Regex.CharClass.{Common => CCC}
import slyce.generation.raw.lexer.nfa.Regex.{CharClass => CC}
import slyce.generation.raw.lexer.nfa.RegexImplicits._
import slyce.generation.raw.lexer.nfa._

object Builder {

  // =====| Lexer |=====

  val nfa: ??[NFA] =
    for {
      // =====| Create Regex |=====
      intR <- CCC.d * (1, None)
      addOp = CC.only('+', '*')
      multOp = CC.only('*', '/')
      powOp = CC.only('^')
      variable <- (CCC.letterUnderscoreNumber * (0, None)).map(r => CCC.lowerLetters =>> r)
      raw = CC.only("=()\n")
      whitespace = CCC.st
      // =====| Create NFA |=====
      nfa = new NFA("Main")
      main = nfa.initialMode
    } yield {
      // =====| Attach to states |=====
      main |~> intR |+ 1 << List(__$("int"))
      main |~> addOp |+ 2 << List(__$("addOp"))
      main |~> multOp |+ 3 << List(__$("multOp"))
      main |~> powOp |+ 4 << List(__$("powOp"))
      main |~> variable |+ 5 << List(__$("variable"))
      main |~> raw |+ 6 << List(__@(None))
      main |~> whitespace |+ 7 << Nil

      nfa
    }

  val grammarErrs: ??[Unit] =
    ???

}
