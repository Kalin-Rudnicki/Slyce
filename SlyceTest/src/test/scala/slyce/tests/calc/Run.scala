package slyce.tests.calc

import slyce.parse.Expression
import slyce.tests.Runner

object Run extends App {
  import Data._

  Runner.run(Parser, "res-test/calc/samples/ex1.txt") { rawTree =>
    println("Success:")
    println()
    println(rawTree)
    println()
    println()
    println()

    def exprRoot(expression: Expression[NonTerminal.Expr.Operand, NonTerminal.Expr.Operator]): String = {
      def operandString(operand: NonTerminal.Expr.Operand): String =
        operand match {
          case NonTerminal.Expr_4._1(_, expr, _) =>
            exprRoot(expr.toExpr)
          case NonTerminal.Expr_4._2(int) =>
            int.text
          case NonTerminal.Expr_4._3(float) =>
            float.text
          case NonTerminal.Expr_4._4(_var) =>
            _var.text
        }

      expression match {
        case Expression.Simple(o) =>
          operandString(o)
        case Expression.Compound(left, op, right) =>
          val operatorString =
            op match {
              case Token.addOp(text, _) =>
                text
              case Token.multOp(text, _) =>
                text
              case Token.powOp(text, _) =>
                text
            }

          s"(${exprRoot(left)} $operatorString ${exprRoot(right)})"
      }
    }

    rawTree.toList.foreach {
      case NonTerminal.Line._3(comment) =>
        println(comment)
      case NonTerminal.Line._2(NonTerminal.Assign._1(_var, _, expr)) =>
        println(s"${_var.text} = ${exprRoot(expr.toExpr)}")
      case NonTerminal.Line._1(expr) =>
        println(s"=> ${exprRoot(expr.toExpr)}")
    }
  }

}
