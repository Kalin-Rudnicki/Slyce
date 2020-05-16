package slyce.lexer

case class Action(lineNo: Int, tokenSpecs: List[TokenSpec], mode: Option[String]) {
  
  def <<(t: List[TokenSpec]): Action =
    new Action(lineNo, t, mode)
  
  def >>(m: String): Action =
    new Action(lineNo, tokenSpecs, Some(m))
  
}

object Action {

  implicit def intToAction(i: Int): Action =
    new Action(i, Nil, None)

}
