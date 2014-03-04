import exastencils.datastructures._
import exastencils.core._
import exastencils.smoother._

object DummyRoot {
  def apply(children : Node*) = new DummyRoot(children.toList)
}
case class DummyRoot(var children : List[Node]) extends Node

object MainWuppertal extends App {
  
  StateManager.root_ = DummyRoot(
      new SmootherNode( "Gs" ))
  
  val strategy = new Strategy("Smoother")
  
  strategy += Transformation("Implement", {
    case s : SmootherNode => {
      if (s.name == "Gs") {
        DBG("Smoother name = " + s.name)
        new GsSmootherNode()
      } else {
        throw new Exception("Invalid smoother name: " + s.name)
        s
      }
    }
  })
  
  strategy.apply
  
  println("\nResulting tree:\n" + StateManager.root_)
}