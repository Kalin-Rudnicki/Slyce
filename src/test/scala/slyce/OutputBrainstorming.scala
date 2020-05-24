package slyce

object OutputBrainstorming {

  object RawGroups {

    sealed trait Opt1
    sealed trait Opt2
    sealed trait Opt3

  }

  object RefactoredGroups {

    sealed trait Opt1
    sealed trait Opt2
    sealed trait Opt3

  }

  trait MyToken extends GeneralToken

  object MyToken {

    case class @@ private (text: String, stats: Stats) extends MyToken

    case class Tok1(stats: Stats) extends MyToken

    case class Tok2 private (i: Int, stats: Stats) extends MyToken {

      def apply(stats: Stats): Tok2 = {
        val converted: Int = stats.text.toInt
        Tok2(converted, stats)
      }
    }

  }

  /*
  type TopLevelRaw =
    RawGroups.Opt1

  type TopLevelRefactored =
    RefactoredGroups.Opt1

  // =====| Public |=====

  def tokenize(chars: Iterator[Char]): MessageAccumulator[RuntimeMessage, List[MyToken]] =
    iteratorToTokens(chars)

  def parseRaw(chars: Iterator[Char]): MessageAccumulator[RuntimeMessage, TopLevelRaw] =
    iteratorToTokens(chars).flatMap(tokensToRaw)

  def parse(chars: Iterator[Char]): MessageAccumulator[RuntimeMessage, TopLevelRefactored] =
    iteratorToTokens(chars).flatMap(tokensToRaw).flatMap(rawToRefactored)

  // =====| Private |=====

  // TODO (KR) :
  private def iteratorToTokens(chars: Iterator[Char]): MessageAccumulator[RuntimeMessage, List[MyToken]] =
    ???

  // TODO (KR) :
  private def tokensToRaw(tokens: List[MyToken]): MessageAccumulator[RuntimeMessage, TopLevelRaw] =
    ???

  // TODO (KR) :
  private def rawToRefactored(raw: TopLevelRaw): MessageAccumulator[RuntimeMessage, TopLevelRefactored] =
    ???
   */

}
