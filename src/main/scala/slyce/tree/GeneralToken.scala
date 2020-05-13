package slyce.tree

trait GeneralToken {
  import GeneralToken._
  
  def stats: Stats
  
}

object GeneralToken {
  import Stats._
  
  final case class Stats(text: String, span: GlobalSpan, start: RelativePos, end: RelativePos)
  
  object Stats {
  
    final case class GlobalSpan(start: Int, end: Int)
  
    final case class RelativePos(lineNo: Int, posInLine: Int)
    
  }
  

}
