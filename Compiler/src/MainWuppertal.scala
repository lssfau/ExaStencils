import exastencils.datastructures._
import exastencils.core._
import exastencils.smoother._

object DummyNode {
  def apply(children : Node*) = new DummyNode(children.toList)
}
case class DummyNode(var children : List[Node]) extends Node

object MainWuppertal extends App {

  
  def foo(n : Integer, z : Integer = 1)( f : Integer => Integer ) = {
    f(n) * z
  }

  println( foo(4, 2) { n => n * 2 } )

  
  StateManager.root_ = DummyNode(
      new SmootherNode( "Gs" ))
  
  val strategy = new Strategy("Smoother")
  
  strategy += Transformation("Implement", {
    case s : SmootherNode => {
      if (s.name == "Gs") {
        DBG("Smoother name = " + s.name)
        new GsSmootherNode()
      } else {
        ERROR("Invalid smoother " + s.name)
        s
      }
    }
  })
  
  strategy.apply
  
  println(StateManager.root_)
}