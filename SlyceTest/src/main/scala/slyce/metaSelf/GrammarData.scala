package slyce.metaSelf

import slyce.generate.{lexer => lex}
import slyce.generate.{grammar => gram}

object GrammarData {

  val lexerData: lex.Data = {
    import lex._
    import Regex.CharClass._

    def continuedName: Regex =
      (
        Inclusive.AZ |
          Inclusive.az |
          Inclusive.d |
          Inclusive('_')
      ).anyAmount

    Data(
      startMode = "General",
      modes = List(
        Data.Mode(
          name = "General",
          lines = List(
            Data.Mode.Line(
              lineNo = 5,
              regex = Inclusive(' ', '\t').atLeastOnce,
              yields = Yields(
                yields = Nil,
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 6,
              regex = Inclusive('(', ')', '.', '?', '|', '\n'),
              yields = Yields(
                yields = List(Yields.Yield.Text.std),
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 7,
              regex = Regex.Sequence(
                Inclusive('"'),
                Regex
                  .Group(
                    Regex.Sequence(
                      Exclusive('\n', '\t', '\\', '"'),
                    ),
                    Regex.Sequence(
                      Inclusive('\\'),
                      Inclusive('\\', 'n', 't', '"'),
                    ),
                  )
                  .atLeastOnce,
                Inclusive('"'),
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("literal")),
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 8,
              regex = Regex.Sequence(
                Inclusive.az,
                continuedName,
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("terminal")),
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 9,
              regex = Regex.Sequence(
                Inclusive.AZ,
                continuedName,
              ),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("nonTerminal")),
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 10,
              regex = Inclusive(':', '^', '+', '*', '~'),
              yields = Yields(
                yields = List(Yields.Yield.Text.std),
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 11,
              regex = Inclusive('<', '>'),
              yields = Yields(
                yields = List(Yields.Yield.Terminal.std("assocDir")),
                toMode = None,
              ),
            ),
            Data.Mode.Line(
              lineNo = 12,
              regex = Regex.Sequence(
                Inclusive('@'),
                Inclusive('s'),
                Inclusive('t'),
                Inclusive('a'),
                Inclusive('r'),
                Inclusive('t'),
              ),
              yields = Yields(
                yields = List(Yields.Yield.Text.std),
                toMode = None,
              ),
            ),
          ),
        ),
      ),
    )
  }
  val grammarData: gram.Data = {
    import gram._
    import Data.{Identifier => Id}
    import Data.{NonTerminal => NT}
    import Data.{Optional => Opt}
    import Data.NT._

    def anyNl: Data.Element =
      ListNT.*.simple(Id.raw("\n"))

    def someNl: Data.Element =
      ListNT.+.simple(Id.raw("\n"))

    Data(
      startNT = "Grammar",
      nts = List(
        // Grammar
        NT(
          name = "Grammar",
          nt = StandardNT.`:`(
            List(
              anyNl,
              Id.raw("@start"),
              Id("nonTerminal"),
              someNl,
              ListNT.*.beforeAfter(
                Id("Nt"),
                someNl,
              ),
              anyNl,
            ),
          ),
        ),
        // Nt
        NT(
          name = "Nt",
          nt = StandardNT.`:`(
            List(
              Id("nonTerminal"),
              Id("NtBase"),
            ),
          ),
        ),
        // NtBase
        NT(
          name = "NtBase",
          nt = StandardNT.^(
            IgnoredList()(Id("StdNtBase"))(),
            IgnoredList()(Id("ListNtBase"))(),
            IgnoredList()(Id("AssocNtBase"))(),
          ),
        ),
        // StdNtBase
        NT(
          name = "StdNtBase",
          nt = StandardNT.`:`(
            List(
              Id("StdOp"),
              ListNT.+.beforeAfter(
                Id("StdRl"),
                someNl,
                Id.raw("|"),
              ),
            ),
          ),
        ),
        // StdRl
        NT(
          name = "StdRl",
          nt = ListNT.*.simple(Id("ElementWExtras")),
        ),
        // ListNtBase
        NT(
          name = "ListNtBase",
          nt = StandardNT.`:`(
            List(
              Id("ListOp"),
              Id("ListBase"),
            ),
          ),
        ),
        // AnonList
        NT(
          name = "AnonList",
          nt = StandardNT.`:`(
            List(
              Id("ListBase"),
              Id("ListOp"),
            ),
          ),
        ),
        // ListBase
        NT(
          name = "ListBase",
          nt = StandardNT.`:`(
            List(
              Id("Element"),
            ),
            List(
              Id.raw("("),
              ListNT.+.simple(Id("ElementWExtras")),
              Id.raw(")"),
            ),
            List(
              Id.raw("("),
              ListNT.+.simple(Id("ElementWExtras")),
              Id.raw("."),
              ListNT.+.simple(Id("ElementWExtras")),
              Id.raw(")"),
            ),
          ),
        ),
        // AssocNtBase
        NT(
          name = "AssocNtBase",
          nt = StandardNT.`:`(
            List(
              Id.raw("~"),
              ListNT.+.beforeAfter(
                Id("AssocElement"),
                someNl,
                Id.raw("|"),
              ),
              Id("StdNtBase"),
            ),
          ),
        ),
        // Element
        NT(
          name = "Element",
          nt = StandardNT.^(
            IgnoredList()(Id("literal"))(),
            IgnoredList()(Id("terminal"))(),
            IgnoredList()(Id("nonTerminal"))(),
            IgnoredList()(Id("AnonList"))(),
          ),
        ),
        // ElementWExtras
        NT(
          name = "ElementWExtras",
          nt = StandardNT.`:`(
            List(
              Opt(Id.raw("^")),
              Id("Element"),
              Opt(Id.raw("?")),
            ),
          ),
        ),
        // AssocElement
        NT(
          name = "AssocElement",
          nt = StandardNT.`:`(
            List(
              Id("assocDir"),
              Id("Element"),
            ),
          ),
        ),
        // StdOp
        NT(
          name = "StdOp",
          nt = StandardNT.^(
            IgnoredList()(Id.raw(":"))(),
            IgnoredList()(Id.raw("^"))(),
          ),
        ),
        // ListOp
        NT(
          name = "ListOp",
          nt = StandardNT.^(
            IgnoredList()(Id.raw("+"))(),
            IgnoredList()(Id.raw("*"))(),
          ),
        ),
      ),
    )
  }

}
