package slyce.tests

object Test extends App {

  /*
  type Formatter[+Res] = Res => String

  type Type[+Err, +Res] = {
    type ErrT <: Err
    type ResT <: Res

    type FormatterT = Formatter[Res]
  }

  final case class State[+T <: Type[_, _]](
      id: Int,
      transitions: Map[Char, Lazy[State[T]]],
      res: Option[T#ResT],
  )

  def getRes[T <: Type[_, _]](
      input: String,
      init: State[T],
      formatter: T#FormatterT,
      noTransition: T#ErrT,
      noRes: T#ErrT,
  ): T#ErrT \/ String = {
    @tailrec
    def loop(
        chars: List[Char],
        state: State[T],
    ): T#ErrT \/ T#ResT =
      chars match {
        case Nil =>
          state.res match {
            case None =>
              noRes.left
            case Some(res) =>
              res.right
          }
        case head :: tail =>
          state.transitions.get(head) match {
            case None =>
              noTransition.left
            case Some(to) =>
              loop(
                tail,
                to.value,
              )
          }
      }

    loop(
      input.toList,
      init,
    ).map(formatter)
  }
   */

}
