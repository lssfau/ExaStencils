package exastencils.app

/// LayerHandler

trait LayerHandler {
  def initialize() : Unit
  def handle() : Unit
  def print() : Unit
  def shutdown() : Unit
}
