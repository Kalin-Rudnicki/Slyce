package slyce.implementations.generation.lexer

case class Data(
    startMode: String,
    modes: List[Data.Mode]
)

object Data {

  final case class Mode(
      name: String,
      lines: List[Mode.Line]
  )

  object Mode {

    final case class Line(
        lineNo: Int,
        regex: Regex,
        yields: Option[Line.Yield],
        toMode: Option[String]
    )

    object Line {

      sealed trait Yield {
        def textRange: (Option[Int], Option[Int])
        def spanRange: (Option[Int], Option[Int])
      }

      object Yield {

        final case class Text(
            textRange: (Option[Int], Option[Int]),
            spanRange: (Option[Int], Option[Int])
        ) extends Yield

        final case class Terminal(
            name: String,
            textRange: (Option[Int], Option[Int]),
            spanRange: (Option[Int], Option[Int])
        ) extends Yield

      }

    }

  }

}
