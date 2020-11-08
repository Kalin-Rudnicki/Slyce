package slyce.parse

import scala.annotation.tailrec

import scalaz.-\/
import scalaz.Scalaz.ToEitherOps
import scalaz.\/
import scalaz.\/-

import slyce.common.helpers.Idt._
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
    ): List[String] \/ RawTree = {
      def printStack(label: String, list: List[(StateMachine.State[Tok, Nt, RawTree], Tok \/ Nt)]): Unit =
        println(
          Group(
            s"$label:",
            Indented(
              list
                .map {
                  case (s, e) =>
                    val eS = e match {
                      case -\/(a) =>
                        a
                      case \/-(b) =>
                        b
                    }

                    Str(s"${s.id} -> $eS")
                },
            ),
          ).build("|   "),
        )

      printStack("loop", stacks.head.stack)

      stacks match {
        case Nil =>
          // TODO (KR) :
          List("Error: TODO (1)").left
        case StateMachine.StackElement(toks, stack) :: stacksT =>
          stack match {
            case Nil =>
              // TODO (KR) :
              List("Error: TODO (2)").left
            case (StateMachine.State(_, acceptF, returnFs, finalReturn), element) :: _ =>
              finalReturn match {
                case None =>
                  val afterReturn: List[StackElement] =
                    returnFs.map { f =>
                      StateMachine.StackElement(
                        toks, { // TODO (KR) : debugging
                          // println
                          // printStack("before", stack)
                          val res = f(stack)
                          // printStack("after", res)
                          res
                        },
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
                        StateMachine.StackElement(toksT, (newState, toksH.left) :: stack) :: afterReturn ::: stacksT,
                      )
                    case _ =>
                      loop(
                        maxTok,
                        afterReturn ::: stacksT,
                      )
                  }
                case Some(f) =>
                  f(stack).right
              }
          }
      }
    }

    val inputH :: inputT = input.map(_.left)
    loop(
      input.head,
      StateMachine.StackElement(
        inputT,
        (augmentedStart, inputH.left) :: Nil,
      ) :: Nil,
    )
  }

}

object StateMachine {

  final case class StackElement[Tok, Nt, RawTree <: Nt](
      queue: List[Tok \/ Nt],
      stack: List[(State[Tok, Nt, RawTree], Tok \/ Nt)],
  )

  final case class State[Tok, Nt, RawTree <: Nt](
      id: Int,
      acceptF: Option[PartialFunction[Tok \/ Nt, State[Tok, Nt, RawTree]]],
      returnFs: List[PartialFunction[
        (List[Tok \/ Nt], List[(State[Tok, Nt, RawTree], Tok \/ Nt)]),
        (List[Tok \/ Nt], List[(State[Tok, Nt, RawTree], Tok \/ Nt)]),
      ]],
      finalReturn: Option[PartialFunction[
        List[(State[Tok, Nt, RawTree], Tok \/ Nt)],
        RawTree,
      ]],
  )

}

//
