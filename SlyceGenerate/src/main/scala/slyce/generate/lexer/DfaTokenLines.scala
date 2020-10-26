package slyce.generate.lexer

import scalaz.Scalaz.ToEitherOps
import scalaz.\/
import slyce.common.helpers._
import slyce.generate.architecture.{lexer => arch}

object DfaTokenLines extends arch.DfaTokenLines[Dfa] {

  override def apply(input: (Dfa, String)): List[String] \/ List[String] = {
    val dfa: Dfa = input._1
    implicit val idt: String = input._2

    def tokString(tokName: String): String =
      s"final case class $tokName(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token"

    val names = dfa.idxOf.toList.flatMap(_._1.yields.toList.flatMap(_.yields.map(_.name))).distinct.sorted

    List(
      List(
        "sealed trait Token extends Dfa.Token",
        "object Token {",
      ),
      idtStrs(names.map(tokString): _*),
      "}" :: Nil,
    ).flatten.right
  }

}
