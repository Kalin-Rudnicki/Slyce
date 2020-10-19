package slyce.implementations.generation.lexer

case class Data(
    startMode: String,
    modes: List[Data.Mode],
)

object Data {

  final case class Mode(
      name: String,
      lines: List[Mode.Line],
  )

  object Mode {

    final case class Line(
        lineNo: Int,
        regex: Regex,
        yields: Yields,
    )

  }

}
