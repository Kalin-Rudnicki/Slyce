package slyce.parse

sealed trait Expression[+Operand, +Operator]
object Expression {

  final case class Simple[+Operand](o: Operand) extends Expression[Operand, Nothing]
  final case class Compound[+Operand, +Operator](
      left: Expression[Operand, Operator],
      op: Operator,
      right: Expression[Operand, Operator],
  ) extends Expression[Operand, Operator]

  def apply[Operand](o: Operand): Expression[Operand, Nothing] =
    Simple(o)

  def apply[Operand, Operator](
      left: Expression[Operand, Operator],
      op: Operator,
      right: Expression[Operand, Operator],
  ): Expression[Operand, Operator] =
    Compound(left, op, right)

}
