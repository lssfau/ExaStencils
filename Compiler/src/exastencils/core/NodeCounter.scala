package exastencils.core

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation.convFromNode

object NodeCounter extends CustomStrategy("internal::NodeCounter") {
  var iteration = 0
  var hits = 0
  var t = new Transformation("Count", { case x => hits += 1; x })
  
  println("nodecounter;strategy;transformation;iteration;nodes\\\\")

  def count(strategy : Option[String], transformation : Option[String]) : Unit = {
    iteration += 1
    hits = 0
    StateManager.applyStandalone(this, t, StateManager.root)

    var sb = new StringBuilder()
    sb.append("nodecounter;")
    sb.append(strategy.getOrElse("unknown").replace(" ", "\\_"))
    sb.append(';')
    sb.append(transformation.getOrElse("unknown").replace(" ", "\\_"))
    sb.append(';')
    sb.append(iteration)
    sb.append(';')
    sb.append(hits)
    sb.append("\\\\")
    println(sb.toString)
  }
  def count(strategy : String, transformation : String) : Unit = count(Some(strategy), Some(transformation))
  def count(strategy : String) : Unit = count(Some(strategy), None)

  def apply() = count(None, None)

  def resetHits() = {
    hits = 0
  }
  
  def reset() = {
    iteration = 0
    hits = 0
  }

}