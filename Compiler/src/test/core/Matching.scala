package test.core

import exastencils.base.ir.IR_BooleanConstant
import exastencils.core._
import exastencils.datastructures._

object Matching {
  def main(args : Array[String]) : Unit = {
    case class TestRoot(var m : Map[Int, Node]) extends Node
    case class MapNode(var m1 : Map[String, Node]) extends Node

    StateManager.root_ = TestRoot(Map((0, MapNode(Map(("a", IR_BooleanConstant(true)), ("b", IR_BooleanConstant(false)), ("c", IR_BooleanConstant(true)))))))

    var s = new DefaultStrategy("MatchingTestStrategy")
    s += new Transformation("replace bools in maps", { case x : IR_BooleanConstant => IR_BooleanConstant(false) })
    s.apply()
    println(StateManager.root_)
  }
}