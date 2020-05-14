package slyce.lexer

import slyce.tree.GeneralToken
import slyce.tree.GeneralToken.Stats
import Action._

sealed trait Action extends HasTokens with HasModeChange {
  
  def lineNo: Int
  
}

object Action {
  
  // =====| HasTokens |=====
  
  sealed trait HasTokens {
    
    def tokenSpecs: List[TokenSpec]
  
  }
  
  sealed trait HasNoTokens extends HasTokens {
  
    override def tokenSpecs: List[TokenSpec] =
      Nil
    
  }
  
  // =====| HasModeChange |=====
  
  sealed trait HasModeChange {
    
    def mode: Option[String]
    
  }
  
  sealed trait HasNoModeChange extends HasModeChange {
    
    override def mode: Option[String] =
      None
    
  }
  
  // =====| Action |=====
  
  import slyce.lexer.{Action => Act}
  
  case class Action(lineNo: Int) extends Act with HasNoTokens with HasNoModeChange
  
  case class ActionTokens(lineNo: Int, tokenSpecs: List[TokenSpec]) extends Act with HasNoModeChange
  
  case class ActionMode(lineNo: Int, mode: Option[String]) extends Act with HasNoTokens
  
  case class ActionTokensMode(lineNo: Int, tokenSpecs: List[TokenSpec], mode: Option[String]) extends Act
  
}
