package slyce.examples.calculator

import scala.language.implicitConversions

import scalaz.NonEmptyList
import scalaz.Scalaz.ToEitherOps

import klib.fp.instances.{given _}
import klib.fp.ops._
import slyce.generation.GenerationMessage.??
import slyce.generation.TokenSpec._
import slyce.generation.raw.grammar._
import slyce.generation.raw.lexer.nfa.Action._
import slyce.generation.raw.lexer.nfa.Regex.CharClass.{Common => CCC}
import slyce.generation.raw.lexer.nfa.Regex.{CharClass => CC}
import slyce.generation.raw.lexer.nfa.RegexImplicits._
import slyce.generation.raw.lexer.nfa._

object Builder {

  // =====| Lexer |=====

  // =====| Create NFA |=====
  val nfa: NFA = NFA("Main")
  val main: Mode = nfa.initialMode

  val createdNfa: ??[NFA] = for {
    s0 <- main |~> CCC.d * (1, None)
    _ = s0 |+ 1 <|< List(__$("int"))
    s1 <- main |~> CC.only('+', '*')
    _ = s1 |+ 2 <|< List(__$("addOp"))
    s2 <- main |~> CC.only('*', '/')
    _ = s2 |+ 3 <|< List(__$("multOp"))
    s3 <- main |~> CC.only('^')
    _ = s3 |+ 4 <|< List(__$("powOp"))
    s4 <- main |~> CCC.lowerLetters ==> CCC.letterUnderscoreNumber * (0, None)
    _ = s4 |+ 5 <|< List(__$("variable"))
    s5 <- main |~> CC.only('=', '(', ')', ';')
    _ = s5 |+ 6 <|< List(__@(None))
    s6 <- main |~> CCC.st
    _ = s6 |+ 7
  } yield nfa

  // =====| Grammar |=====

  val createdGrammar: ??[Grammar] =
    Grammar(
      NonTerminal.ListNT(
        "Statements",
        Production.ListProduction.SimpleListAny(
          Production.UnwrapProduction(
            Nil,
            "Statement",
            Nil
          )
        )
      ),
      NonTerminal.BasicNT(
        "statement",
        NonEmptyList(
          Production.BasicProduction(
            "variable".right,
            "=".left,
            "Expr".right,
            ";".left
          )
        )
      ),
      NonTerminal.OpNT(
        "Expr",
        NonEmptyList(
          Production.OpProduction(
            Production.OpProduction.Type.Binary,
            Production.OpProduction.Assoc.Right,
            "powOp".right,
          ),
          Production.OpProduction.binLeft("multOp".right),
          Production.OpProduction.binLeft("addOp".right),
        ),
        NonEmptyList(
          Production.BasicProduction("variable".right),
          Production.BasicProduction("int".right),
          Production.BasicProduction(
            "(".left,
            "Expr".right,
            ")".left,
          )
        )
      )
    )

}
