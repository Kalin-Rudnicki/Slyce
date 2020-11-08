package slyce.generate.formatting

import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.\/

import slyce.common.helpers.CharOps
import slyce.common.helpers.Idt
import slyce.common.helpers.Idt._
import slyce.generate.architecture.{formatting => arch}
import slyce.generate.grammar._

object ParseStateLines extends arch.ParseStateLines[StateMachine] {

  override def apply(input: StateMachine): List[String] \/ Idt = {
    val rlMap = input.rlMap

    Group(
      s"val stateMachine: StateMachine[Token, NonTerminal, NonTerminal.${input.startNt}] = {",
      Indented(
        s"type State = StateMachine.State[Token, NonTerminal, NonTerminal.${input.startNt}]",
        Break,
        rlMap.values.toList.sortBy(_.id).map {
          case StateMachine.State(id, rl) =>
            Group(
              s"lazy val s$id: State =",
              Indented(
                "StateMachine.State(",
                Indented(
                  s"id = $id,",
                  rl.accepts.isEmpty.fold(
                    Str("acceptF = None,"),
                    Group(
                      "acceptF = Some {",
                      Indented(
                        rl.accepts.toList.map {
                          case (id, rl2) =>
                            val name = id match {
                              case SimpleData.Identifier.Raw(text) =>
                                s"-\\/(_: Token.${SimpleData.Identifier.RawName}.`${text.map(_.unesc).mkString}`)"
                              case SimpleData.Identifier.Terminal(name)
                                  if name.str.contains(SimpleData.Identifier.EofName) =>
                                s"-\\/(Token.${SimpleData.Identifier.EofName})"
                              case SimpleData.Identifier.Terminal(name) =>
                                s"-\\/(_: Token.${name.str})"
                              case SimpleData.Identifier.NonTerminal(name) =>
                                s"\\/-(_: NonTerminal.${name.str})"
                            }
                            Str(s"case $name => s${rlMap(rl2).id}")
                        },
                      ),
                      "},",
                    ),
                  ),
                  rl.returns.isEmpty.fold(
                    Str("returnFs = Nil,"),
                    Group(
                      "returnFs = List(",
                      Indented(
                        rl.returns.toList.map {
                          case (name, i, elements) =>
                            elements.isEmpty.fold(
                              Group(
                                "{",
                                Indented(
                                  "case list @ ((s, _) :: _) =>",
                                  Indented(
                                    s"// ${name.str}[$i] : ${elements.map(_.str).mkString(", ")}",
                                    s"(s, NonTerminal.${name.str}._$i.right) :: list",
                                  ),
                                ),
                                "},",
                              ),
                              Group(
                                "{",
                                Indented(
                                  "case list =>",
                                  Indented(
                                    s"// ${name.str}[$i] : ${elements.map(_.str).mkString(", ")}",
                                    "???", // TODO (KR) :
                                  ),
                                ),
                                "},",
                              ),
                            )
                        },
                      ),
                      "),",
                    ),
                  ),
                  (id == 0).fold(
                    Group(
                      "finalReturn = None, // TODO : ...",
                    ),
                    "finalReturn = None,",
                  ),
                ),
                ")",
              ),
              Break,
            )
        },
        "StateMachine(s0)",
      ),
      "}",
    ).right
  }

}
