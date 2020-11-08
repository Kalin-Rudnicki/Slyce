package slyce.parse

import scala.annotation.tailrec

import scalaz.-\/
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.ToOptionOpsFromOption
import scalaz.\/
import scalaz.\/-

import slyce.common.helpers.Matcher
import slyce.common.helpers.TraverseOps
import slyce.parse.{architecture => arch}

final case class StateMachine[Tok, Nt, RawTree <: Nt](
    augmentedStart: StateMachine.State[Tok, Nt, RawTree],
) extends arch.Grammar[Tok, List[String], RawTree] {

  type State = StateMachine.State[Tok, Nt, RawTree]
  type StackElement = StateMachine.StackElement[Tok, Nt, RawTree]

  override def apply(input: List[Tok]): List[String] \/ RawTree = {
    @tailrec
    def loop(
        maxTok: Tok,
        stacks: List[StackElement],
    ): List[String] \/ RawTree =
      stacks match {
        case Nil =>
          // TODO (KR) :
          List("Error: TODO").left
        case StateMachine.StackElement(toks, stack) :: stacksT =>
          stack match {
            case Nil =>
              // TODO (KR) :
              List("Error: TODO").left
            case (StateMachine.State(acceptF, returnFs, finalReturn), element) :: stackT =>
              finalReturn match {
                case None =>
                  val afterReturn: List[StackElement] =
                    returnFs.map { f =>
                      StateMachine.StackElement(
                        toks,
                        f(stack),
                      )
                    }

                  acceptF match {
                    case None =>
                      loop(
                        maxTok,
                        afterReturn ::: stacksT,
                      )
                    case Some(f) if f.isDefinedAt(element) =>
                      val newState = f(element)
                      val toksH :: toksT = toks
                      loop(
                        // TODO (KR) : This is not correct, need a way to find which token is max
                        toksH,
                        StateMachine.StackElement(toksT, (newState, toksH.left) :: stackT) :: afterReturn ::: stacksT,
                      )
                    case _ =>
                      // TODO (KR) :
                      List("Error: TODO").left
                  }
                case Some(f) =>
                  f(stack).right
              }
          }
      }

    val inputH :: inputT = input
    loop(
      inputH,
      StateMachine.StackElement(
        inputT,
        (augmentedStart, inputH.left) :: Nil,
      ) :: Nil,
    )
  }

}

object StateMachine {

  final case class StackElement[Tok, Nt, RawTree <: Nt](
      toks: List[Tok],
      stack: List[(State[Tok, Nt, RawTree], Tok \/ Nt)],
  )

  final case class State[Tok, Nt, RawTree <: Nt](
      acceptF: Option[PartialFunction[Tok \/ Nt, State[Tok, Nt, RawTree]]],
      returnFs: List[PartialFunction[
        List[(State[Tok, Nt, RawTree], Tok \/ Nt)],
        List[(State[Tok, Nt, RawTree], Tok \/ Nt)],
      ]],
      finalReturn: Option[PartialFunction[
        List[(State[Tok, Nt, RawTree], Tok \/ Nt)],
        RawTree,
      ]],
  )

}
