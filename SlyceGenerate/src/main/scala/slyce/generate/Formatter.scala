package slyce.generate

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scala.annotation.tailrec

import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.\/

import klib.Idt
import klib.Idt._
import klib.CharStringOps._
import slyce.generate.{architecture => arch}
import slyce.generate.{grammar => gram}
import gram.SimpleData
import slyce.generate.{lexer => lex}

object Formatter extends arch.Formatter[lex.Dfa, gram.SimpleData, gram.StateMachine, List[String]] {

  override def apply(
      input: (lex.Dfa, grammar.SimpleData, grammar.StateMachine),
  ): List[String] \/ (arch.Formatter.Settings => String) = { (settings: arch.Formatter.Settings) =>
    val (dfa, simpleData, stateMachine) = input

    val tokenLines: Idt = {
      import gram.SimpleData.Identifier
      import lex.Yields.Yield

      def tokString(text: Boolean, tokName: String, id: Identifier): Idt = {
        val extendsOpsStr =
          simpleData.extendsOps
            .get(id)
            .toSet
            .flatten
            .map(n => s" with NonTerminal.${n.str}.Operator")
            .mkString
        s"final case class $tokName(${text.fold("text: String, ", "")}span: Dfa.Token.Span) extends HasSpanToken$extendsOpsStr"
      }

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

      val rawNames: List[Identifier.Raw] =
        simpleData.reductionLists.flatMap {
          _.reductions.list.toList.flatMap {
            _.elements.flatMap {
              case raw: Identifier.Raw =>
                raw.some
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
          rawNames.nonEmpty.option(
            Group(
              s"object ${Identifier.RawName} {",
              Break,
              Indented(
                "def apply(str: String, span: Dfa.Token.Span): Token =",
                Indented(
                  "str match {",
                  Indented(
                    rawNames.distinct.map { rn =>
                      val n = rn.text
                      Group(
                        f"case ${n.unesc} =>",
                        Indented(s"${n.unesc("`")}.apply(span)"),
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
                rawNames.map(rn => tokString(false, rn.text.unesc("`"), rn)),
              ),
              "}",
            ),
          ),
          names.map(n => tokString(true, n, Identifier.Terminal(n))),
        ),
        "}",
        "object HasSpanToken {",
        Indented(
          "def unapply(arg: HasSpanToken): Option[Dfa.Token.Span] = arg.span.some",
        ),
        "}",
      )
    }

    val (ntLines: Idt, ntLinesNeedsTailRec: Boolean) = {
      import gram.SimpleData.Identifier

      def formatRL(list: gram.SimpleData.ReductionList): (Idt, Boolean) = {
        import gram.SimpleData.ReductionList.Simplifiers

        val name = list.name.str

        def formatR(r: gram.SimpleData.ReductionList.Reduction): Idt =
          r.elements.isEmpty.fold(
            Group(
              s"case object _${r.idx} extends $name",
              Break,
            ),
            Group(
              s"final case class _${r.idx}(",
              Indented(
                r.elements.zipWithIndex.map {
                  case (e, i) =>
                    val `type` =
                      e match {
                        case Identifier.Raw(text) =>
                          s"Token.${Identifier.RawName}.${text.unesc("`")}"
                        case Identifier.Terminal(name) =>
                          s"Token.$name"
                        case Identifier.NonTerminal(name) =>
                          s"NonTerminal.${name.str}"
                      }

                    Str(s"_${i + 1}: ${`type`},")
                },
              ),
              s") extends $name",
              Break,
            ),
          )

        def formatSimplifiers(simp: Simplifiers): Idt = {
          def formatOptional(s: Identifier): Idt = {
            val toName =
              s match {
                case Identifier.Raw(text) =>
                  s"Token.${Identifier.RawName}.${text.unesc("`")}"
                case Identifier.Terminal(name) =>
                  s"Token.$name"
                case Identifier.NonTerminal(name) =>
                  s"NonTerminal.${name.str}"
              }

            Group(
              s"def toOption: Option[$toName] =",
              Indented(
                "this match {",
                Indented(
                  s"case $name._1(n) =>",
                  Indented("n.some"),
                  s"case $name._2 =>",
                  Indented("None"),
                ),
                "}",
              ),
            )
          }

          def formatList(s: Simplifiers.ListSimplifier): Idt = {
            val toName =
              s.`type` match {
                case Identifier.Raw(text) =>
                  s"Token.${Identifier.RawName}.${text.unesc("`")}"
                case Identifier.Terminal(name) =>
                  s"Token.$name"
                case Identifier.NonTerminal(name) =>
                  s"NonTerminal.${name.str}"
              }

            def convertPositions(p: Simplifiers.ListSimplifier.Positions): String = {
              val nonTail = 0.to(p.total).toList.map {
                case i if i == p.lift =>
                  "n"
                case _ =>
                  "_"
              }

              s"${nonTail.mkString(", ")}, tail"
            }

            Group(
              s"def toList: List[$toName] = {",
              Indented(
                s._2 match {
                  case None =>
                    Group(
                      "@tailrec",
                      s"def loop(unseen: $name, seen: List[$toName]): List[$toName] =",
                      Indented(
                        "unseen match {",
                        Indented(
                          s"case $name._1(${convertPositions(s._1)}) =>",
                          Indented("loop(tail, n :: seen)"),
                          s"case $name._2 =>",
                          Indented("seen.reverse"),
                        ),
                        "}",
                        Break,
                        "loop(this, Nil)",
                      ),
                    )
                  case Some(_2) =>
                    val nextName = list.name.next.str
                    Group(
                      "@tailrec",
                      s"def loop(unseen: $nextName, seen: List[$toName]): List[$toName] =",
                      Indented(
                        "unseen match {",
                        Indented(
                          s"case $nextName._1(${convertPositions(_2)}) =>",
                          Indented("loop(tail, n :: seen)"),
                          s"case $nextName._2 =>",
                          Indented("seen.reverse"),
                        ),
                      ),
                      "}",
                      Break,
                      Group(
                        "this match {",
                        Indented(
                          s"case $name._1(${convertPositions(s._1)}) =>",
                          Indented("loop(tail, n :: Nil)"),
                          s._1CanBeEmpty.option(
                            Group(
                              s"case $name._2 =>",
                              Indented("Nil"),
                            ),
                          ),
                        ),
                        "}",
                      ),
                    )
                },
              ),
              "}",
            )
          }

          def formatExpr(s: Simplifiers.ExprSimplifier): Idt =
            Group(
              s"def toExpr: Expression[${s.rootName.str}.Operand, ${s.rootName.str}.Operator] =",
              Indented(
                s.opId.nonEmpty.fold(
                  Group(
                    "this match {",
                    Indented(
                      s"case ${list.name.str}._1(left, op, right) =>",
                      Indented("Expression(left.toExpr, op, right.toExpr)"),
                      s"case ${list.name.str}._2(child) =>",
                      Indented("child.toExpr"),
                    ),
                    "}",
                  ),
                  "Expression(this)",
                ),
              ),
            )

          List(
            simp.optional.map(formatOptional),
            simp.list.map(formatList),
            simp.expr.map(formatExpr),
          ).flatten.flatMap(_ :: Break :: Nil)
        }

        val extendsOpsStr =
          simpleData.extendsOps
            .get(SimpleData.Identifier.NonTerminal(list.name))
            .toSet
            .flatten
            .map(n => s" with ${n.str}.Operator")
            .mkString
        val traitSig = s"sealed trait $name extends NonTerminal$extendsOpsStr"

        (
          Group(
            list.simplifiers.nonEmpty.fold(
              Group(
                s"$traitSig {",
                Indented(
                  Break,
                  formatSimplifiers(list.simplifiers),
                ),
                "}",
              ),
              traitSig,
            ),
            s"object $name {",
            Indented(
              list.simplifiers.expr.filter(_.rootName == list.name).map { s =>
                Group(
                  Break,
                  s"sealed trait Operator",
                  s"type Operand = ${s.baseName.str}",
                )
              },
              Break,
              list.reductions.list.toList.map(formatR),
            ),
            "}",
            Break,
          ),
          list.simplifiers.list.nonEmpty,
        )
      }

      val sorted = simpleData.reductionLists.sortBy(_.name)(ord = {
        case (_1, _2) =>
          import SimpleData.Name

          _1 match {
            case _1: Name.AnonList =>
              _2 match {
                case _2: Name.AnonList =>
                  _1.str.compareTo(_2.str)
                case _: Name.Optional =>
                  1
                case _: Name.Named =>
                  -1
              }
            case _1: Name.Optional =>
              _2 match {
                case _: Name.AnonList =>
                  -1
                case _2: Name.Optional =>
                  _1.str.compareTo(_2.str)
                case _: Name.Named =>
                  -1
              }
            case _1: Name.Named =>
              _2 match {
                case _: Name.AnonList =>
                  1
                case _: Name.Optional =>
                  1
                case _2: Name.Named =>
                  _1.str.compareTo(_2.str)
              }
          }
      })
      val mapped = sorted.map(formatRL)

      (
        Group(
          "sealed trait NonTerminal",
          "object NonTerminal {",
          Indented(
            Break,
            mapped.map(_._1),
          ),
          "}",
        ),
        mapped.exists(_._2),
      )
    }

    val dfaLines: Idt = {
      import gram.SimpleData.Identifier.EofName
      import lex.Yields.Yield

      def stateName(state: lex.Dfa.State): String =
        s"s${dfa.idxOf(state)}"

      def stateStringLines(state: lex.Dfa.State): Idt = {
        def lazyName(state: lex.Dfa.State): String =
          s"Lazy(${stateName(state)})"

        Group(
          s"lazy val ${stateName(state)}: Dfa.State[Token] =",
          Indented(
            "Dfa.State(",
            Indented(
              s"id = ${dfa.idxOf(state)},",
            ),
            state.transitions.isEmpty.fold(
              Indented(
                "transitions = Map.empty,",
              ),
              Indented(
                "transitions = Map(",
                Indented(
                  state.transitions.toList
                    .flatMap {
                      case (ss, s) =>
                        ss.map(_ -> s)
                    }
                    .sortBy(_._1)
                    .map {
                      case (c, s) =>
                        // Seems like the safest way to avoid all sorts of weird character escapes
                        Str(s"0x${c.toInt.toHexString.toUpperCase}.toChar -> ${s.map(lazyName)}, // ${c.unesc}")
                    },
                ),
                "),",
              ),
            ),
            Indented(
              s"elseTransition = ${state.elseTransition.map(lazyName)},",
            ),
            state.yields.fold(
              Indented(
                "yields = None,",
              ),
            ) { yields =>
              yields.yields.isEmpty.fold(
                Indented(
                  s"yields = Some(Dfa.State.Yields(${stateName(yields.toMode)})()),",
                ),
                Indented(
                  s"yields = Some(",
                  Indented(
                    s"Dfa.State.Yields(${stateName(yields.toMode)})(",
                    Indented(
                      yields.yields.map { y =>
                        Group(
                          "Dfa.State.Yields.Yield(",
                          Indented(
                            s"tokF = Token.${y.name}.apply,",
                            s"spanRange = ${y.spanRange},",
                          ),
                          "),",
                        )
                      },
                    ),
                    "),",
                  ),
                  "),",
                ),
              )
            },
            ")",
          ),
        )
      }

      Group(
        "private val lexer: Dfa[Token] = {",
        Indented(dfa.idxOf.toList.sortBy(_._2).map(p => stateStringLines(p._1))),
        Break,
        Indented(s"Dfa(${stateName(dfa.initialState)}, Token.$EofName)"),
        "}",
      )
    }

    val parseStateLines: Idt = {
      import gram.SimpleData.Identifier
      import gram.SimpleData.Identifier.RawName
      import gram.SimpleData.Identifier.EofName

      val rlMap = stateMachine.rlMap

      def patternName(id: Identifier, name: Option[String]): String = {
        val n = name.getOrElse("_")
        val eofN = name.fold("")(n => s"$n @ ")
        id match {
          case Identifier.Raw(text) =>
            s"-\\/($n: ${rawName(text)})"
          case Identifier.Terminal(name) if name.contains(EofName) =>
            s"-\\/(${eofN}Token.$EofName)"
          case Identifier.Terminal(name) =>
            s"-\\/($n: Token.$name)"
          case Identifier.NonTerminal(name) =>
            s"\\/-($n: NonTerminal.${name.str})"
        }
      }

      def rawName(text: String): String =
        s"Token.$RawName.${text.unesc("`")}"

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
        s"private val grammar: Builder[Token, NonTerminal, NonTerminal.${stateMachine.startNt}]#StateMachine =",
        Indented(
          s"Builder.builder[Token, NonTerminal, NonTerminal.${stateMachine.startNt}].build { builder =>",
          Indented(
            matcher("elem", ("ElementT", 3)),
            matcher("stateElem", ("State", 1), ("ElementT", 3)),
            Break,
            rlMap.toList
              .map(_._2)
              .sortBy(_.id)
              .map {
                case gram.StateMachine.State(id, rl) =>
                  val (_returns, generates) = rl.returns.toList.partitionMap {
                    case rl2 @ (_, _, ids) =>
                      ids.isEmpty.fold(
                        rl2.right.toEither,
                        rl2.left.toEither,
                      )
                  }
                  val (returns, _finalReturns) = _returns.partitionMap {
                    case rl2 @ (gram.SimpleData.Name.Start, _, _) =>
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
                                      todo: List[gram.SimpleData.Identifier],
                                      seen: List[String],
                                  ): String =
                                    todo match {
                                      case Nil =>
                                        s"case ${seen.mkString(" :: ")} :: stackT =>"
                                      case todoH :: todoT =>
                                        val inner = patternName(todoH, s"_$idx".some)
                                        val wrapped =
                                          idx match {
                                            case 1 =>
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
                            val wholePattern = s"${patterns.reverse.map(p => s"elem($p)").mkString(" :: ")} :: Nil"

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
      )
    }

    val importTailRec = ntLinesNeedsTailRec

    Group(
      Group(
        s"// DO NOT EDIT : Automatically generated by Slyce v${slyce.Version} @ ${DateTimeFormatter.ofPattern("MM/dd/yyyy").format(LocalDate.now())}",
        s"package ${settings.packageName.mkString(".")}",
        Break,
        importTailRec.option(
          Group(
            "import scala.annotation.tailrec",
            Break,
          ),
        ),
        "import scalaz.\\/",
        "import scalaz.-\\/",
        "import scalaz.\\/-",
        "import scalaz.Scalaz.ToBooleanOpsFromBoolean",
        "import scalaz.Scalaz.ToOptionIdOps",
        Break,
        "import slyce.common.helpers._",
        "import slyce.parse._",
        "import slyce.parse.{architecture => arch}",
      ),
      Break,
      Group(
        "object Data {",
        Indented(
          Break,
          tokenLines,
          Break,
          ntLines,
          Break,
        ),
        "}",
      ),
      Break,
      Group(
        s"object ${settings.className} extends arch.Parser[String, List[String], Data.NonTerminal.${simpleData.startNt}] {",
        Indented(
          "import Data._",
          Break,
          dfaLines,
          Break,
          parseStateLines,
          Break,
          Group(
            s"override def apply(input: String): List[String] \\/ NonTerminal.${simpleData.startNt} =",
            Indented("arch.Parser(lexer, grammar)(input)"),
          ),
          Break,
        ),
        "}",
      ),
    ).build(settings.indent)
  }.right

}
