package exastencils.datastructures

import exastencils.core.StateManager
import exastencils.core.Logger._
import exastencils.core._

abstract class CustomStrategy(name : String) extends Strategy(name) {
  protected var token : Option[StateManager.TokenType] = None
  
  def apply() : Unit
  
  def begin() = {
    token = Some(StateManager.transaction(this))
    Logger.info(s"""Applying strategy "${name}"""")
  }

  def commit() = {
    StateManager.commit(token.get)
  }
  
  def abort() = {
    StateManager.abort(token.get)
  }
}