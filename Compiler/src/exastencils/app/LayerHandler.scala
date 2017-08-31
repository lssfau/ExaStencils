package exastencils.app

/// LayerHandler

trait LayerHandler {
  def initialize() : Unit
  def handle() : Unit
  def shutdown() : Unit
}
