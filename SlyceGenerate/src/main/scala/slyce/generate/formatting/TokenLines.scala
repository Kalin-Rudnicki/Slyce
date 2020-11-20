package slyce.generate.formatting

import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.\/

import slyce.common.helpers._
import slyce.common.helpers.Idt._
import slyce.generate.architecture.{formatting => arch}
import slyce.generate.grammar._
import slyce.generate.grammar.SimpleData.Identifier
import slyce.generate.lexer._
import slyce.generate.lexer.Yields.Yield

object TokenLines extends arch.TokenLines[Dfa, SimpleData] {

  override def apply(input: (Dfa, SimpleData)): List[String] \/ Idt = {
    val (
      dfa: Dfa,
      sd: SimpleData,
    ) = input

    def tokString(text: Boolean, tokName: String): Idt =
      s"final case class $tokName(${text.fold("text: String, ", "")}span: Dfa.Token.Span) extends HasSpanToken"

    val names: List[String] =
      dfa.idxOf.toList
        .flatMap {
          _._1.yields.toList.flatMap {
            _.yields.flatMap {
              case Yield.Terminal(name, _) =>
                name.some
              case _ =>
                None
            }
          }
        }
        .distinct
        .sorted

    val rawNames: List[String] =
      sd.reductionLists.flatMap {
        _.reductions.list.toList.flatMap {
          _.elements.flatMap {
            case Identifier.Raw(text) =>
              text.some
            case _ =>
              None
          }
        }
      }.distinct

    Group(
      "sealed trait Token extends Dfa.Token",
      "sealed trait HasSpanToken extends Token with Dfa.Token.HasSpan",
      "object Token {",
      Indented(
        s"case object ${Identifier.EofName} extends Token",
        rawNames.isEmpty.fold(
          Group(),
          Group(
            s"object ${Identifier.RawName} {",
            Break,
            Indented(
              "def apply(str: String, span: Dfa.Token.Span): Token =",
              Indented(
                "str match {",
                Indented(
                  rawNames.distinct.map { n =>
                    Group(
                      f"case ${n.unesc} =>",
                      Indented(s"`${n.map(_.unesc).mkString}`(span)"),
                    )
                  },
                  Group(
                    "case _ =>",
                    Indented("???"),
                  ),
                ),
                "}",
              ),
              Break,
              rawNames.map(r => tokString(false, s"`${r.map(_.unesc).mkString}`")),
            ),
            "}",
          ),
        ),
        names.map(tokString(true, _)),
      ),
      "}",
      "object HasSpanToken {",
      Indented(
        "def unapply(arg: HasSpanToken): Option[Dfa.Token.Span] = arg.span.some",
      ),
      "}",
    ).right
  }

}
