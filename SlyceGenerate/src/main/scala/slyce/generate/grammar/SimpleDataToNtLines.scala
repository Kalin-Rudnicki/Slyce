package slyce.generate.grammar

import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.\/

import slyce.common.helpers._
import slyce.generate.architecture.{grammar => arch}

object SimpleDataToNtLines extends arch.SimpleDataToNtLines[SimpleData] {

  override def apply(input: (SimpleData, String)): List[String] \/ List[String] = {
    val data: SimpleData = input._1
    implicit val idt: String = input._2

    List(
      "object NonTerminal {" :: Nil,
      idtLists(
        List("") ::
          data.reductionLists.map { list =>
            List(
              s"sealed trait ${list.name.str}" :: s"object ${list.name.str} {" :: Nil,
              idtLists(
                List("") ::
                  list.reductions.list.toList.zipWithIndex.map {
                    case (r, i) =>
                      r.elements.isEmpty.fold(
                        s"case object _$i extends ${list.name.str}" :: "" :: Nil,
                        List(
                          s"final case class _$i(" :: Nil,
                          idtStrs(
                            r.elements.zipWithIndex.map {
                              case (e, i2) =>
                                val t =
                                  e match {
                                    case SimpleData.Identifier.Raw(_) =>
                                      "Token.raw"
                                    case SimpleData.Identifier.Terminal(name) =>
                                      s"Token.$name"
                                    case SimpleData.Identifier.NonTerminal(name) =>
                                      s"NonTerminal.${name.str}"
                                  }

                                s"_$i2: $t,"
                            }: _*,
                          ),
                          s") extends ${list.name.str}" :: "" :: Nil,
                        ).flatten,
                      )
                  }: _*,
              ),
              "}" :: "" :: Nil,
            ).flatten
          }: _*,
      ),
      "}" :: Nil,
    ).flatten.right
  }

}
