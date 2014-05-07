package test.core

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._

object Matching {
  def main(args : Array[String]) : Unit = {
    case class TestRoot(var m : Map[Int, Node]) extends Node
    case class MapNode(var m1 : Map[String, Node]) extends Node
    
    StateManager.root_ = TestRoot(Map((0, MapNode(Map(("a", BooleanConstant(true)), ("b", BooleanConstant(false)), ("c", BooleanConstant(true)))))))

    var s = new Strategy("MatchingTestStrategy")
    s += new Transformation("replace bools in maps", { case x : BooleanConstant => BooleanConstant(false) })
    s.apply()
    println(StateManager.root_)
  }
}