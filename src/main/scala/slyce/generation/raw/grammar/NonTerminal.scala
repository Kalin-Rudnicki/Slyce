package slyce.generation.raw.grammar

import scala.language.implicitConversions

import scalaz.NonEmptyList
import scalaz.Scalaz.ToOptionIdOps

import slyce.generation.raw.grammar.Production._

sealed trait NonTerminal {
  def name: String
}

object NonTerminal {

  case class BasicNT(name: String, productions: NonEmptyList[BasicProduction]) extends NonTerminal

  case class UnwrapNT(name: String) extends NonTerminal

  case class ListNT(name: String, production: ListProduction) extends NonTerminal

  case class OpNT(
      name: String,
      opProductions: NonEmptyList[OpProduction] ,
      basicProductions: NonEmptyList[BasicProduction] 
  ) extends NonTerminal

}
