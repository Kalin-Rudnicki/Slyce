package slyce.generate.lexer

import scalaz.Scalaz.ToEitherOps
import scalaz.\/
import slyce.common.helpers._
import slyce.common.helpers.Idt._
import slyce.generate.architecture.{lexer => arch}

object DfaTokenLines extends arch.DfaTokenLines[Dfa] {

  override def apply(dfa: Dfa): List[String] \/ Idt = {
    def tokString(tokName: String): Idt =
      s"final case class $tokName(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token"

    val names = dfa.idxOf.toList.flatMap(_._1.yields.toList.flatMap(_.yields.map(_.name))).distinct.sorted

    Group(
      "sealed trait Token extends Dfa.Token",
      "object Token {",
      Indented(names.map(tokString)),
      "}",
    ).right
  }

}
