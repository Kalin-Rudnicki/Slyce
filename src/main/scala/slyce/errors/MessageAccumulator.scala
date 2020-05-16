package slyce.errors

import scalaz.Scalaz._

import slyce.lexer.dfa.GenerationMessage

sealed trait MessageAccumulator[+E <: GenerationMessage, +T] {

  import MessageAccumulator._

  // =====| Implement these... |=====

  def msgs: List[E]

  def messages: List[E] =
    msgs.reverse

  def <<[E2 >: E <: GenerationMessage](moreMessages: E2*): MessageAccumulator[E2, T]

  def map[T2](f: T => T2): MessageAccumulator[E, T2]

  def flatMap[E2 >: E <: GenerationMessage, T2](
      f: T => MessageAccumulator[E2, T2]
  ): MessageAccumulator[E2, T2]

  // =====| ... and you get these for free |=====

  def toOption: Option[T] =
    this match {
      case Alive(_, v) =>
        v.some
      case _ =>
        None
    }

  def sort[E2 >: E <: GenerationMessage](implicit
      sorter: MessageSorter[E2]
  ): (SortedMessages[E2], Option[T]) = {
    val sorted: SortedMessages[E2] = MessageType.sort(messages)
    (
      sorted,
      toOption.flatMap(v => sorted.error.isEmpty.option(v))
    )
  }

  def <@>[T2](f: T => T2): MessageAccumulator[E, T2] =
    this.map(f)

  def <#>[E2 >: E <: GenerationMessage, T2](
      f: T => MessageAccumulator[E2, T2]
  ): MessageAccumulator[E2, T2] =
    this.flatMap(f)

}

object MessageAccumulator {

  // =====| Implicit helpers |=====

  object implicits {

    implicit class MessageAccumulatorOps[T](v: T) {

      // TODO (KR) : Rename

      def value: MessageAccumulator[Nothing, T] =
        valueM()

      def valueM[E <: GenerationMessage](messages: E*): MessageAccumulator[E, T] =
        Alive(v, messages: _*)

    }

    implicit class FMessageAccumulatorOps[T1, T2](f: T1 => T2) {

      def apply[E <: GenerationMessage](messages: E*): T1 => MessageAccumulator[E, T2] =
        v => Alive(f(v), messages: _*)

    }

  }

  // =====| Value |=====

  final class Alive[E <: GenerationMessage, T] private[MessageAccumulator] (
      val msgs: List[E],
      private val value: T
  ) extends MessageAccumulator[E, T] {

    override def map[T2](f: T => T2): MessageAccumulator[E, T2] =
      new Alive(msgs, f(value))

    override def flatMap[E2 >: E <: GenerationMessage, T2](
        f: T => MessageAccumulator[E2, T2]
    ): MessageAccumulator[E2, T2] =
      f(value) match {
        case Alive(m, v) =>
          new Alive[E2, T2](m ::: msgs, v)
        case Dead(m) =>
          new Dead[E2](m ::: msgs)
      }

    override def <<[E2 >: E <: GenerationMessage](moreMessages: E2*): MessageAccumulator[E2, T] =
      new Alive[E2, T](moreMessages.toList.reverse ::: msgs, value)

    override def toString: String =
      s"Value($value)(${messages.mkString(", ")})"

  }

  object Alive {

    def apply[E <: GenerationMessage, T](value: T, messages: E*): Alive[E, T] =
      new Alive[E, T](messages.toList.reverse, value)

    def unapply[E <: GenerationMessage, T](a: Alive[E, T]): Option[(List[E], T)] =
      (a.msgs, a.value).some

  }

  // =====| NoValue |=====

  final class Dead[E <: GenerationMessage] private[MessageAccumulator] (val msgs: List[E])
      extends MessageAccumulator[E, Nothing] {

    override def map[T2](f: Nothing => T2): MessageAccumulator[E, T2] =
      this

    override def flatMap[E2 >: E <: GenerationMessage, T2](
        f: Nothing => MessageAccumulator[E2, T2]
    ): MessageAccumulator[E2, T2] =
      this

    override def <<[E2 >: E <: GenerationMessage](
        moreMessages: E2*
    ): MessageAccumulator[E2, Nothing] =
      new Dead[E2](moreMessages.toList.reverse ::: msgs)

    override def toString: String =
      s"NoValue(${messages.mkString(", ")})"

  }

  object Dead {

    def apply[E <: GenerationMessage](m0: E, messages: E*): Dead[E] =
      new Dead[E]((m0 :: messages.toList).reverse)

    def unapply[E <: GenerationMessage](a: Dead[E]): Option[List[E]] =
      a.msgs.some

  }

  // =====| MessageHandler |=====

  trait MessageSorter[-E <: GenerationMessage] {

    def sort(e: E): MessageType.Value

  }

  object MessageType extends Enumeration {

    def sort[E <: GenerationMessage](
        list: List[E]
    )(implicit sorter: MessageSorter[E]): SortedMessages[E] = {
      val sorted: Map[MessageType.Value, List[E]] =
        list.map(m => (sorter.sort(m), m)).groupMap(_._1)(_._2)
      SortedMessages[E](
        sorted.getOrElse(Ignore, Nil),
        sorted.getOrElse(Debug, Nil),
        sorted.getOrElse(Info, Nil),
        sorted.getOrElse(Warning, Nil),
        sorted.getOrElse(Error, Nil)
      )
    }

    // =====| Enums |=====

    val Ignore, Debug, Info, Warning, Error = Value

  }

  final case class SortedMessages[E <: GenerationMessage](
      ignore: List[E],
      debug: List[E],
      info: List[E],
      warning: List[E],
      error: List[E]
  )

}
