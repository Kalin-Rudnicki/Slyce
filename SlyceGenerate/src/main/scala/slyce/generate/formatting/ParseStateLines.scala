package slyce.generate.formatting

import scala.annotation.tailrec

import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.\/

import slyce.common.helpers.CharOps
import slyce.common.helpers.Idt
import slyce.common.helpers.Idt._
import slyce.generate.architecture.{formatting => arch}
import slyce.generate.grammar.SimpleData.Identifier
import slyce.generate.grammar._

object ParseStateLines extends arch.ParseStateLines[StateMachine] {

  override def apply(input: StateMachine): List[String] \/ Idt = {
    val rlMap = input.rlMap

    import SimpleData.Identifier.RawName
    import SimpleData.Identifier.EofName

    def patternName(id: SimpleData.Identifier, name: Option[String]): String = {
      val n = name.getOrElse("_")
      val eofN = name.fold("")(n => s"$n @ ")
      id match {
        case SimpleData.Identifier.Raw(text) =>
          s"-\\/($n: ${rawName(text)})"
        case SimpleData.Identifier.Terminal(name) if name.contains(EofName) =>
          s"-\\/(${eofN}Token.$EofName)"
        case SimpleData.Identifier.Terminal(name) =>
          s"-\\/($n: Token.$name)"
        case SimpleData.Identifier.NonTerminal(name) =>
          s"\\/-($n: NonTerminal.${name.str})"
      }
    }

    def rawName(text: String): String =
      s"Token.$RawName.`${text.map(_.unesc).mkString}`"

    def matcher(name: String, matchOn: (String, Int)*): Idt = {
      val (
        tosStr,
        idxsFStr,
      ) = matchOn.toList match {
        case (to, idx) :: Nil =>
          (
            s"builder.$to",
            s"(_._$idx)",
          )
        case pairs =>
          (
            s"(${pairs.map(_._1).map(to => s"builder.$to").mkString(", ")})", {
              val idxs = pairs.map(_._2)
              val all = List(1, 2, 3)
              val params = s"(${all.map(idx => idxs.contains(idx).fold(s"_$idx", "_")).mkString(", ")})"
              val res = s"(${idxs.map(idx => s"_$idx").mkString(", ")})"
              s" { case $params => $res }"
            },
          )
      }

      Group(
        s"val $name: Matcher[builder.StackFrame.StackElement, $tosStr] = { element =>",
        Indented(
          s"builder.StackFrame.StackElement.unapply(element).map$idxsFStr",
        ),
        "}",
      )
    }

    Group(
      s"val stateMachine: Builder[Token, NonTerminal, NonTerminal.${input.startNt}]#StateMachine =",
      Indented(
        s"Builder.builder[Token, NonTerminal, NonTerminal.${input.startNt}].build { builder =>",
        Indented(
          matcher("elem", ("ElementT", 3)),
          matcher("stateElem", ("State", 1), ("ElementT", 3)),
          Break,
          rlMap.toList
            .map(_._2)
            .sortBy(_.id)
            .map {
              case StateMachine.State(id, rl) =>
                val (_returns, generates) = rl.returns.toList.partitionMap {
                  case rl2 @ (_, _, ids) =>
                    ids.isEmpty.fold(
                      rl2.right.toEither,
                      rl2.left.toEither,
                    )
                }
                val (returns, _finalReturns) = _returns.partitionMap {
                  case rl2 @ (SimpleData.Name.Start, _, _) =>
                    rl2.right.toEither
                  case rl2 =>
                    rl2.left.toEither
                }
                val finalReturn = _finalReturns.headOption

                Group(
                  s"lazy val s$id: builder.State =",
                  Indented(
                    "builder.State(",
                    Indented(
                      s"id = $id,",
                      rl.accepts.isEmpty.fold(
                        Str("acceptF = None,"),
                        Group(
                          "acceptF = Some {",
                          Indented(
                            rl.accepts.toList.map {
                              case (id, rl2) =>
                                val to = rlMap(rl2)
                                val pattern = patternName(id, None)

                                Str(s"case $pattern => s${to.id}")
                            },
                          ),
                          "},",
                        ),
                      ),
                      returns.isEmpty.fold(
                        Str("returnFs = Nil,"),
                        Group(
                          "returnFs = List(",
                          Indented(
                            returns.map {
                              case (name, i, ids) =>
                                @tailrec
                                def buildPatternName(
                                    idx: Int,
                                    todo: List[SimpleData.Identifier],
                                    seen: List[String],
                                ): String =
                                  todo match {
                                    case Nil =>
                                      s"case ${seen.mkString(" :: ")} :: stackT =>"
                                    case todoH :: todoT =>
                                      val inner = patternName(todoH, s"_$idx".some)
                                      val wrapped =
                                        todoT match {
                                          case Nil =>
                                            s"stateElem(state, $inner)"
                                          case _ =>
                                            s"elem($inner)"
                                        }

                                      buildPatternName(
                                        idx + 1,
                                        todoT,
                                        wrapped :: seen,
                                      )
                                  }

                                Group(
                                  "{",
                                  Indented(
                                    buildPatternName(1, ids, Nil),
                                    Indented(
                                      "(",
                                      Indented(
                                        "state,",
                                        s"NonTerminal.${name.str}._$i(${1.to(ids.length).map(s => s"_$s").mkString(", ")}),",
                                        "stackT,",
                                      ),
                                      ")",
                                    ),
                                  ),
                                  "},",
                                )
                            },
                          ),
                          "),",
                        ),
                      ),
                      generates.isEmpty.fold(
                        Str("spontaneouslyGenerates = Nil,"),
                        Group(
                          "spontaneouslyGenerates = List(",
                          Indented(
                            generates.map {
                              case (name, i, _) =>
                                Str(s"NonTerminal.${name.str}._$i,")
                            },
                          ),
                          "),",
                        ),
                      ),
                      finalReturn.fold(
                        Str("finalReturnF = None,"): Idt,
                      ) {
                        case (_, _, ids) =>
                          val h :: t = ids
                          val patterns = patternName(h, "rawTree".some) :: t.map(patternName(_, None))
                          val wholePattern = s"${patterns.map(p => s"elem($p)").mkString(" :: ")} :: Nil"

                          Group(
                            "finalReturnF = Some {",
                            Indented(
                              s"case $wholePattern =>",
                              Indented(
                                "rawTree",
                              ),
                            ),
                            "},",
                          )
                      },
                    ),
                    ")",
                  ),
                )
            },
          Break,
          "s0",
        ),
        "} {",
        Indented(
          "case (t1 @ HasSpanToken(s1), t2 @ HasSpanToken(s2)) =>",
          Indented(
            "(s2.start.abs > s1.start.abs).fold(t2, t1)",
          ),
          s"case (eof @ Token.${Identifier.EofName}, _) =>",
          Indented(
            "eof",
          ),
          s"case (_, eof) =>",
          Indented(
            "eof",
          ),
        ),
        "}",
      ),
    ).right
  }

}
