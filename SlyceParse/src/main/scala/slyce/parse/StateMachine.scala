package slyce.parse

import scala.annotation.tailrec

import scalaz.-\/
import scalaz.\/
import scalaz.\/-
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.ToOptionOpsFromOption

import klib.CharStringOps._
import klib.ColorString.syntax.auto.toSimpleColorString
import slyce.common.helpers.TraverseOps
import slyce.parse.{architecture => arch}

final class Builder[Tok, Nt, RawTree <: Nt] private {

  // =====| Build |=====

  def build(f: this.type => State)(maxTokF: (Tok, Tok) => Tok): StateMachine =
    StateMachine(maxTokF, f(this))

  // =====| Helper types |=====

  type PF[I, O] = {
    type InputT = I
    type OutputT = O
    type T = I => Option[O]
  }

  type ElementT = Tok \/ Nt
  type QueueT = List[ElementT]
  type StackT = List[StackFrame.StackElement]

  type AcceptF = PF[
    ElementT,
    State,
  ]
  type ReturnF = PF[
    StackT,
    (
        State, // State to transition to
        Nt, // NonTerminal that was produced
        StackT, // Remaining stack elements
    ),
  ]
  type FinalReturnF = PF[
    StackT,
    RawTree,
  ]

  // =====| State & StateMachine |=====

  final case class State(
      id: Int,
      acceptF: Option[AcceptF#T],
      returnFs: List[ReturnF#T],
      spontaneouslyGenerates: List[Nt],
      finalReturnF: Option[FinalReturnF#T],
  )

  final case class StackFrame private (
      queue: QueueT,
      stack: StackT,
  )
  object StackFrame {

    final case class StackElement private (
        state: State,
        canReturn: Boolean,
        element: ElementT,
    )

  }

  // =====| StateMachine |=====

  final case class StateMachine private[Builder] (
      maxTokF: (Tok, Tok) => Tok,
      augmentedStart: State,
  ) extends arch.Grammar[Tok, List[String], RawTree] {

    override def apply(input: List[Tok]): List[String] \/ RawTree = {

      // Errors

      def frameworkError_(msgs: String*): List[String] =
        msgs.toList.map(m => s"(Framework Error) $m")

      def frameworkError(msgs: String*): List[String] \/ Nothing =
        frameworkError_(msgs: _*).left

      def userError_(msgs: String*): List[String] =
        msgs.toList.map(m => s"(User Error) $m")

      def userError(msgs: String*): List[String] \/ Nothing =
        userError_(msgs: _*).left

      @tailrec
      def loop(
          maxTok: Tok,
          frames: List[StackFrame],
      ): List[String] \/ RawTree = {
        // TODO (KR) : =====| Debug |=====
        def stackFrameToIdt(sf: StackFrame): klib.Idt = {
          import klib.Idt._

          def elementToString(element: ElementT): String = {
            val tmp = element match {
              case null =>
                "null" // TODO (KR) : Remove this
              case -\/(o) =>
                o.toString.unesc("")
              case \/-(o) =>
                s"${o.getClass.getName.split("\\.").last.split("\\$").toList.tail.tail.head} : ${o.toString.unesc("")}"
            }

            tmp
              .replaceAll("terminal", "terminal".red.toString)
              .replaceAll("nonTerminal", "nonTerminal".red.toString)
          }

          Group(
            // "queue =>",
            {
              val limit: Option[Int] = 5.some

              limit.cata(
                lim =>
                  Indented(
                    (sf.queue.size > lim) ? (Str("...".yellow.toString): Idt) | Group(),
                    sf.queue.take(lim).reverse.map(e => Str(elementToString(e))),
                  ),
                Indented(
                  sf.queue.reverse.map(e => Str(elementToString(e))),
                ),
              )
            },
            "stack =>",
            Indented(
              sf.stack.map {
                case StackFrame.StackElement(s, b, e) =>
                  Str(s"(${s.id}, $b) => ${elementToString(e)}")
              },
            ),
          )
        }

        // Helpers

        def callAcceptF(
            f: Option[AcceptF#T],
            frameQueue: QueueT,
            frameStackH: StackFrame.StackElement,
            frameStackT: StackT,
        ): List[String] \/ List[StackFrame] = {
          val StackFrame.StackElement(_, _, arg) = frameStackH
          f.flatMap(_(arg)) match {
            case Some(toState) =>
              def newStackFrame(
                  frameQueueH: ElementT,
                  frameQueueT: QueueT,
              ): StackFrame =
                StackFrame(
                  queue = frameQueueT,
                  stack =
                    StackFrame.StackElement(
                      state = toState,
                      canReturn = true,
                      element = frameQueueH,
                    ) :: frameStackH :: frameStackT,
                )

              frameQueue match {
                case Nil =>
                  // TODO (KR) : If this actually ends up working... FIX IT!!!
                  // frameworkError("No elements in queue to pop")
                  (
                    newStackFrame(
                      null, // TODO (KR) : Flagrant foul
                      Nil,
                    ) :: Nil
                  ).right
                case fQH :: fQT =>
                  (
                    newStackFrame(
                      fQH,
                      fQT,
                    ) :: Nil
                  ).right
              }
            case None =>
              Nil.right
          }
        }

        def callReturnFs(
            fs: List[ReturnF#T],
            frameQueue: QueueT,
            frameStackH: StackFrame.StackElement,
            frameStackT: StackT,
        ): List[String] \/ List[StackFrame] = {
          val StackFrame.StackElement(_, _, elem) = frameStackH

          def createStackFrame(res: ReturnF#OutputT): StackFrame =
            res match {
              case (state, nt, frameStackR) =>
                StackFrame(
                  queue = elem :: frameQueue,
                  stack =
                    StackFrame.StackElement(
                      state = state,
                      canReturn = false,
                      element = nt.right,
                    ) :: frameStackR,
                )
            }

          fs.map(_(frameStackT).map(createStackFrame) \/> frameworkError_("Failed to call `returnF`")).traverseErrs
        }

        def spontaneouslyGenerate(
            nts: List[Nt],
            frameQueue: QueueT,
            frameStackH: StackFrame.StackElement,
            frameStackT: StackT,
        ): List[StackFrame] =
          nts.map { nt =>
            val StackFrame.StackElement(state, _, element) = frameStackH
            StackFrame(
              queue = element :: frameQueue,
              stack =
                StackFrame.StackElement(
                  state = state,
                  canReturn = false,
                  element = nt.right,
                ) :: frameStackT,
            )
          }

        def callFinalReturnF(
            f: FinalReturnF#T,
            arg: FinalReturnF#InputT,
        ): List[String] \/ RawTree =
          f(arg) \/> frameworkError_("Failed to call `finalReturnF`")

        // Loop

        frames match {
          case Nil =>
            userError(s"Unable to build parse-tree. MaxTok: $maxTok")
          case StackFrame(queue, stack) :: framesT =>
            stack match {
              case Nil =>
                frameworkError("StackFrame has empty stack")
              case (
                    stackH @ StackFrame.StackElement(
                      State(_, acceptF, returnFs, spontaneouslyGenerates, finalReturnF),
                      canReturn,
                      _,
                    )
                  ) :: stackT =>
                finalReturnF match {
                  case Some(f) =>
                    callFinalReturnF(f, stackT)
                  case None =>
                    val newFrames: List[String] \/ List[StackFrame] =
                      for {
                        framesFromAccept <- callAcceptF(acceptF, queue, stackH, stackT)
                        framesFromReturn <- canReturn.fold(
                          callReturnFs(returnFs, queue, stackH, stackT),
                          Nil.right,
                        )
                        framesFromSpontaneousGeneration = canReturn.fold(
                          spontaneouslyGenerate(spontaneouslyGenerates, queue, stackH, stackT),
                          Nil,
                        )
                      } yield {
                        List(
                          framesFromAccept,
                          framesFromReturn,
                          framesFromSpontaneousGeneration,
                          framesT,
                        ).flatten
                      }

                    newFrames match {
                      case \/-(newFrames) =>
                        val newMaxTok: Tok =
                          newFrames match {
                            case StackFrame(_, StackFrame.StackElement(_, _, -\/(newTok)) :: _) :: _ =>
                              maxTokF(maxTok, newTok)
                            case _ =>
                              maxTok
                          }

                        loop(
                          newMaxTok,
                          newFrames,
                        )
                      case error @ -\/(_) =>
                        error
                    }
                }
            }
        }
      }

      input match {
        case Nil =>
          frameworkError("Somehow received no tokens...")
        case toksH :: toksT =>
          loop(
            maxTok = toksH,
            frames =
              StackFrame(
                queue = toksT.map(_.left),
                stack =
                  StackFrame.StackElement(
                    state = augmentedStart,
                    canReturn = true,
                    element = toksH.left,
                  ) :: Nil,
              ) :: Nil,
          )
      }
    }

  }

}

object Builder {

  def builder[Tok, Nt, RawTree <: Nt]: Builder[Tok, Nt, RawTree] =
    new Builder

}
