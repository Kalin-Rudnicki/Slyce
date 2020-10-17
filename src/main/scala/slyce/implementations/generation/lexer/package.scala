package slyce.implementations.generation

package object lexer {

  type Err = List[String]

  final case class Yields(
      yields: Option[Yields.Yield],
      toMode: Option[String]
  )

  object Yields {

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
