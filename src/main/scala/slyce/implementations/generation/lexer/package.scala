package slyce.implementations.generation

package object lexer {

  type Err = List[String]

  final case class Yields(
      // TODO (KR) : Make this a List instead, allowing for multiple Yield
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
      object Text {

        def std: Text =
          Text((None, None), (None, None))

      }

      final case class Terminal(
          name: String,
          textRange: (Option[Int], Option[Int]),
          spanRange: (Option[Int], Option[Int])
      ) extends Yield
      object Terminal {

        def std(name: String): Terminal =
          Terminal(name, (None, None), (None, None))

      }

    }

  }

}
