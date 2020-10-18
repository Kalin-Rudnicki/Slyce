package slyce.implementations.generation

package object lexer {

  type Err = List[String]

  final case class Yields(
      yields: List[Yields.Yield],
      toMode: Option[String]
  )

  object Yields {

    sealed trait Yield {
      def name: String
      def spanRange: (Int, Int)
      def textRange: (Int, Int)
    }

    object Yield {

      final case class Range(
      )

      final case class Text(
          spanRange: (Int, Int),
          textRange: (Int, Int)
      ) extends Yield {
        val name: String = "Text"
      }
      object Text {

        def std: Text =
          Text((0, -1), (0, -1))

      }

      final case class Terminal(
          name: String,
          spanRange: (Int, Int),
          textRange: (Int, Int)
      ) extends Yield
      object Terminal {

        def std(name: String): Terminal =
          Terminal(name, (0, -1), (0, -1))

      }

    }

  }

}
