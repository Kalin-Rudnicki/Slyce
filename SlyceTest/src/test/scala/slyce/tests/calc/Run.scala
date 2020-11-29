package slyce.tests.calc

import scala.annotation.tailrec

import scalaz.-\/
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.ToOptionOpsFromOption
import scalaz.\/
import scalaz.\/-

import slyce.parse.Expression
import slyce.Runner

object Run extends App {
  import Data._

  import klib.ColorString.syntax._
  import auto._
  import klib.Idt._
  import klib.Logger.GlobalLogger

  implicit val flags: Set[String] = Set()

  Runner.run(Parser, "res-test/calc/samples/ex1.txt") { rawTree =>
    val lines: List[NonTerminal.Line] =
      rawTree match {
        case NonTerminal.Lines._1(_, lines, _) =>
          lines.toList
      }

    type NumberT = Int \/ Float

    @tailrec
    def loop(
        todo: List[NonTerminal.Line],
        map: Map[String, NumberT],
    ): Unit = {
      def evalExpr(expr: Expression[NonTerminal.Expr.Operand, NonTerminal.Expr.Operator]): List[String] \/ NumberT =
        expr match {
          case Expression.Simple(o) =>
            o match {
              case NonTerminal.Expr_4._1(_, expr2, _) =>
                evalExpr(expr2.toExpr)
              case NonTerminal.Expr_4._2(int) =>
                int.text.toInt.left.right
              case NonTerminal.Expr_4._3(float) =>
                float.text.toFloat.right.right
              case NonTerminal.Expr_4._4(_var) =>
                map.get(_var.text) \/> List(s"No such variable: ${_var.text} @ ${_var.span.start.pos}")
            }
          case Expression.Compound(left, op, right) =>
            val (opStr, pos) = op match {
              case Token.addOp(text, span) =>
                (text, span.start)
              case Token.multOp(text, span) =>
                (text, span.start)
              case Token.powOp(text, span) =>
                (text, span.start)
            }

            for {
              leftR <- evalExpr(left)
              rightR <- evalExpr(right)
              joined <- {
                def applyF(
                    intF: (Int, Int) => (List[String] \/ NumberT),
                )(floatF: (Float, Float) => (List[String] \/ NumberT)): List[String] \/ NumberT =
                  (leftR, rightR) match {
                    case (-\/(leftR), -\/(rightR)) =>
                      intF(leftR, rightR)
                    case (-\/(leftR), \/-(rightR)) =>
                      floatF(leftR, rightR)
                    case (\/-(leftR), -\/(rightR)) =>
                      floatF(leftR, rightR)
                    case (\/-(leftR), \/-(rightR)) =>
                      floatF(leftR, rightR)
                  }

                opStr match {
                  case "+" =>
                    applyF((l, r) => (l + r).left.right)((l, r) => (l + r).right.right)
                  case "-" =>
                    applyF((l, r) => (l + r).left.right)((l, r) => (l + r).right.right)
                  case "*" =>
                    applyF((l, r) => (l * r).left.right)((l, r) => (l * r).right.right)
                  case "/" =>
                    applyF { (l, r) =>
                      (r == 0).option((l / r).left) \/> List(s"Div / 0 @ ${pos.pos}")
                    } { (l, r) =>
                      (r == 0).option((l / r).right) \/> List(s"Div / 0 @ ${pos.pos}")
                    }
                  case "^" =>
                    applyF((l, r) => Math.pow(l, r).toFloat.right.right)((l, r) => Math.pow(l, r).toFloat.right.right)
                  case _ =>
                    List(s"Unknown operator: $opStr").left
                }
              }
            } yield joined
        }

      todo match {
        case Nil =>
        case line :: next =>
          val mapVal: Option[Map[String, NumberT]] =
            line match {
              case NonTerminal.Line._1(expr) =>
                evalExpr(expr.toExpr) match {
                  case e @ -\/(errs) =>
                    GlobalLogger.fatal("Error(s):")
                    errs.foreach(GlobalLogger.error(_))
                    None
                  case \/-(num) =>
                    GlobalLogger.print(num.fold(_.toString, _.toString))
                    map.some
                }
              case NonTerminal.Line._2(NonTerminal.Assign._1(_var, _, expr)) =>
                evalExpr(expr.toExpr) match {
                  case e @ -\/(errs) =>
                    GlobalLogger.fatal("Error(s):")
                    errs.foreach(GlobalLogger.error(_))
                    None
                  case \/-(num) =>
                    GlobalLogger.info(s"${_var.text} = ${num.fold(_.toString, _.toString)}")
                    (map + (_var.text -> num)).some
                }
              case NonTerminal.Line._3(comment) =>
                GlobalLogger.detailed("Comment:")
                GlobalLogger.detailed(comment.text)
                map.some
            }

          mapVal match {
            case None =>
            case Some(newMap) =>
              loop(next, newMap)
          }
      }
    }

    GlobalLogger.break
    GlobalLogger.debug("=====| Calc.Run |=====")
    loop(lines, Map())

  }

}
