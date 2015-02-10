package test.core

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.logger._

object RemoveNodes {
  case class Wurzel(var a : A, var aopt : Option[A], var b : B, var bopt : Option[B]) extends Node
  case class A(var one : scala.collection.immutable.HashSet[B],
    var two : scala.collection.mutable.HashSet[B],
    var three : scala.collection.immutable.List[B],
    var four : scala.collection.mutable.ListBuffer[B]) extends Node
  case class B(var zahl : Integer) extends Node

  def main(args : Array[String]) : Unit = {

    var tree = Wurzel(
      A(
        scala.collection.immutable.HashSet(B(1), B(2), B(3)),
        scala.collection.mutable.HashSet(B(11), B(12), B(13)),
        scala.collection.immutable.List(B(21), B(22), B(23)),
        scala.collection.mutable.ListBuffer(B(31), B(32), B(33))),
      Some(A(
        scala.collection.immutable.HashSet(B(1), B(2), B(3)),
        scala.collection.mutable.HashSet(B(11), B(12), B(13)),
        scala.collection.immutable.List(B(21), B(22), B(23)),
        scala.collection.mutable.ListBuffer(B(31), B(32), B(33)))),
      B(41),
      Some(B(42)))

    StateManager.root_ = tree
    var s = new DefaultStrategy("MatchingTestStrategy")
    s += new Transformation("remove in im.HashSets", { case x : B if (x.zahl % 2 == 0) => x })

    Logger.setLevel(Logger.INFO)

    println(s"""before: ${StateManager.root_}""")
    s.apply()
    println(s"""after:  ${StateManager.root_}""")

  }
}