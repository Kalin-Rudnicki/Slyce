package slyce.generation.raw.grammar

import scalaz.NonEmptyList
import scalaz.std.option.optionSyntax._

import slyce.generation.raw.grammar.Production._

sealed trait NonTerminal {

  def name: String

}

object NonTerminal {

  class BasicNT(val name: String) extends NonTerminal {

    private var productions: Option[NonEmptyList[BasicProduction]] = None

    def init(productions: NonEmptyList[BasicProduction]): Unit =
      this.productions = productions.some

  }

  object BasicNT {

    def unapply(arg: BasicNT): Option[(String, NonEmptyList[BasicProduction])] =
      arg.productions.map((arg.name, _))

  }

  class UnwrapNT(val name: String) extends NonTerminal {

    var productions: Option[NonEmptyList[UnwrapProduction]] = None

    def init(productions: NonEmptyList[UnwrapProduction]): Unit =
      this.productions = productions.some

  }

  object UnwrapNT {

    def unapply(arg: UnwrapNT): Option[(String, NonEmptyList[UnwrapProduction])] =
      arg.productions.map((arg.name, _))

  }

  class ListNT(val name: String) extends NonTerminal {

    private var production: Option[ListProduction] = None

    def init(production: ListProduction): Unit =
      this.production = production.some

  }

  object ListNT {

    def unapply(arg: ListNT): Option[(String, ListProduction)] =
      arg.production.map((arg.name, _))

  }

  class OpNT(val name: String) extends NonTerminal {

    private var opProductions: Option[NonEmptyList[OpProduction]] = None
    private var basicProductions: Option[NonEmptyList[BasicProduction]] = None

    def init(
        opProductions: NonEmptyList[OpProduction],
        basicProductions: NonEmptyList[BasicProduction]
    ): Unit = {
      this.opProductions = opProductions.some
      this.basicProductions = basicProductions.some
    }

  }

  object OpNT {

    def unapply(arg: OpNT): Option[(String, NonEmptyList[OpProduction], NonEmptyList[BasicProduction])] =
      arg.opProductions.flatMap { opP =>
        arg.basicProductions.map { bP =>
          (arg.name, opP, bP)
        }
      }

  }

}
