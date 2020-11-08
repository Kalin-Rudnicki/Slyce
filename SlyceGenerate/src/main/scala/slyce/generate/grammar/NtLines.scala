package slyce.generate.grammar

import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.\/

import slyce.common.helpers.CharOps
import slyce.common.helpers.Idt
import slyce.common.helpers.Idt._
import slyce.generate.architecture.{formatting => arch}
import slyce.generate.grammar.SimpleData.Identifier

object NtLines extends arch.NtLines[SimpleData] {

  override def apply(input: SimpleData): List[String] \/ Idt =
    Group(
      "sealed trait NonTerminal",
      "object NonTerminal {",
      Break,
      Indented(
        input.reductionLists.map { list =>
          val name = list.name.str
          Group(
            s"sealed trait $name extends NonTerminal",
            s"object $name {",
            Break,
            list.reductions.list.toList.map {
              r =>
                r.elements.isEmpty.fold(
                  Indented(
                    s"case object _${r.idx} extends $name",
                    Break,
                  ),
                  Indented(
                    s"final case class _${r.idx}(",
                    Indented(
                      r.elements.zipWithIndex.map {
                        case (e, i) =>
                          val `type` =
                            e match {
                              case SimpleData.Identifier.Raw(text) =>
                                s"Token.${Identifier.RawName}.`${text.map(_.unesc).mkString}`"
                              case SimpleData.Identifier.Terminal(name) =>
                                s"Token.$name"
                              case SimpleData.Identifier.NonTerminal(name) =>
                                s"NonTerminal.${name.str}"
                            }

                          Str(s"_${i + 1}: ${`type`},")
                      },
                    ),
                    s") extends $name",
                    Break,
                  ),
                )
            },
            "}",
            Break,
          )
        },
      ),
      "}",
    ).right

}
