package slyce.parse

import scala.annotation.tailrec

import scalaz.-\/
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.\/
import scalaz.\/-

import slyce.common.helpers.TraverseOps
import slyce.parse.{architecture => arch}

final class Builder[Tok, Nt, RawTree <: Nt] private {

  // =====| Build |=====

  def build(maxTokF: (Tok, Tok) => Tok)(f: this.type => State): StateMachine =
    StateMachine(maxTokF, f(this))

  // =====| Helper types |=====

  type PF[I, O] = {
    type InputT = I
    type OutputT = O
    type T = PartialFunction[I, O]
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
        canSpontaneouslyGenerate: Boolean,
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

      def frameworkError(msgs: String*): List[String] \/ Nothing =
        msgs.toList.map(m => s"(Framework Error) $m").left

      def userError(msgs: String*): List[String] \/ Nothing =
        msgs.toList.map(m => s"(User Error) $m").left

      @tailrec
      def loop(
          maxTok: Tok,
          frames: List[StackFrame],
      ): List[String] \/ RawTree = {

        // Helpers

        def callAcceptF(
            f: Option[AcceptF#T],
            frameQueue: QueueT,
            frameStackH: StackFrame.StackElement,
            frameStackT: StackT,
        ): List[String] \/ List[StackFrame] = {
          val StackFrame.StackElement(_, _, arg) = frameStackH
          f match {
            case Some(f) if f.isDefinedAt(arg) =>
              def createStackFrame(
                  res: AcceptF#OutputT,
                  frameQueueH: ElementT,
                  frameQueueT: QueueT,
              ): StackFrame =
                StackFrame(
                  queue = frameQueueT,
                  stack =
                    StackFrame.StackElement(
                      state = res,
                      canSpontaneouslyGenerate = true,
                      element = frameQueueH,
                    ) :: frameStackH :: frameStackT,
                )

              frameQueue match {
                case Nil =>
                  frameworkError("No elements in queue to pop")
                case fQH :: fQT =>
                  (
                    createStackFrame(
                      f(arg),
                      fQH,
                      fQT,
                    ) :: Nil
                  ).right
              }
            case _ =>
              Nil.right
          }
        }

        def callReturnFs(
            fs: List[ReturnF#T],
            frameQueue: QueueT,
            frameStack: StackT,
        ): List[String] \/ List[StackFrame] = {
          def createStackFrame(res: ReturnF#OutputT): StackFrame =
            res match {
              case (state, nt, frameStackR) =>
                StackFrame(
                  queue = frameQueue,
                  stack =
                    StackFrame.StackElement(
                      state = state,
                      canSpontaneouslyGenerate = true, // TODO (KR) : It is possible this should be false
                      element = nt.right,
                    ) :: frameStackR,
                )
            }

          fs.map {
            case f if f.isDefinedAt(frameStack) =>
              createStackFrame(f(frameStack)).right
            case _ =>
              frameworkError("Unable to call `returnF`")
          }.traverseErrs
        }

        def spontaneouslyGenerate(
            nts: List[Nt],
            // extras
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
                  canSpontaneouslyGenerate = false,
                  element = nt.right,
                ) :: frameStackT,
            )
          }

        def callFinalReturnF(
            f: FinalReturnF#T,
            arg: FinalReturnF#InputT,
        ): List[String] \/ RawTree =
          if (f.isDefinedAt(arg))
            f(arg).right
          else
            frameworkError("Failed to call `finalReturnF`")

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
                      canSpontaneouslyGenerate,
                      element,
                    )
                  ) :: stackT =>
                finalReturnF match {
                  case Some(f) =>
                    callFinalReturnF(f, stack)
                  case None =>
                    val newFrames: List[String] \/ List[StackFrame] =
                      for {
                        framesFromAccept <- callAcceptF(acceptF, queue, stackH, stackT)
                        framesFromReturn <- callReturnFs(returnFs, queue, stack)
                        framesFromSpontaneousGeneration = canSpontaneouslyGenerate.fold(
                          spontaneouslyGenerate(spontaneouslyGenerates, queue, stackH, stackT),
                          Nil,
                        )
                      } yield List(
                        framesFromAccept,
                        framesFromReturn,
                        framesFromSpontaneousGeneration,
                        framesT,
                      ).flatten

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
                    canSpontaneouslyGenerate = true, // TODO (KR) : It is possible this should be false
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
