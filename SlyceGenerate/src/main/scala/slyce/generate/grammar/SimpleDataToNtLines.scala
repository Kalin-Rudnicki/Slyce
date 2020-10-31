package slyce.generate.grammar

import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.\/

import slyce.common.helpers.Idt
import slyce.common.helpers.Idt._
import slyce.generate.architecture.{grammar => arch}

object SimpleDataToNtLines extends arch.SimpleDataToNtLines[SimpleData] {

  override def apply(input: SimpleData): List[String] \/ Idt =
    Group(
      "object NonTerminal {",
      Break,
      Indented(
        input.reductionLists.map { list =>
          val name = list.name.str
          Group(
            s"sealed trait $name",
            s"object $name {",
            Break,
            list.reductions.list.toList.zipWithIndex.map {
              case (r, i1) =>
                r.elements.isEmpty.fold(
                  Indented(
                    s"case object _${i1 + 1} extends $name",
                    Break,
                  ),
                  Indented(
                    s"final case class _${i1 + 1}(",
                    Indented(
                      r.elements.zipWithIndex.map {
                        case (e, i2) =>
                          val `type` =
                            e match {
                              case SimpleData.Identifier.Raw(_) =>
                                "Token.raw"
                              case SimpleData.Identifier.Terminal(name) =>
                                s"Token.$name"
                              case SimpleData.Identifier.NonTerminal(name) =>
                                s"NonTerminal.${name.str}"
                            }

                          Str(s"_${i2 + 1}: ${`type`},")
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
