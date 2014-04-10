package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

abstract class Statement extends Node

case class VariableDeclarationStatement(var identifier : String, var datatype : Datatype, var expression : Option[Expression] = None)
  extends Statement

case class DomainDeclarationStatement(name : String) extends Statement

case class AssignmentStatement(var identifier : Identifier, var expression : Expression)
  extends Statement

case class LoopOverDomainStatement(iterationset : String, blocksize : Option[Index], var statements : List[Statement])
  extends Statement

case class IterationSetDeclarationStatement(identifier : Identifier, begin : Index, end : Index, increment : Option[Index]) extends Statement

case class FunctionStatement(var identifier : Identifier, var returntype : Datatype, var arguments : List[Variable], var statements : List[Statement])
  extends Statement

case class RepeatUpStatement(number : Int, var statements : List[Statement]) extends Statement

case class RepeatUntilStatement(var comparison : BooleanExpression, var statements : List[Statement]) extends Statement

case class ReductionStatement(var statement : Statement) extends Statement

case class FunctionCallStatement(identifier : Identifier, var arguments : List[Expression]) extends Statement

case class ConditionalStatement(var expression : BooleanExpression, var statements : List[Statement]) extends Statement

case class FieldDeclarationStatement(name : String, var datatype : Datatype, var offset : Index, var level : Option[LevelSpecification]) extends Statement with ProgressableToIr {
  var communicate = false
  var ghostlayers = 0
  var padding = 0
  var slots = 1
  var bcDir0 = false
  def set(t : TempOption) { // FIXME hack
    t.key match {
      case "communicate" => communicate = t.value.toBoolean
      case "ghostlayers" => ghostlayers = t.value.toInt
      case "padding"     => padding = t.value.toInt
      case "slots"       => slots = t.value.toInt
      case "bcDir"       => bcDir0 = t.value.toBoolean
      case _             => exastencils.core.ERROR(s"Unknown option ${t.key} = ${t.value}")
    }
  }
  
   def progressToIr : Field ={
        val layout = DimArray().map(dim => new FieldLayoutPerDim(if (0 == dim) 1 else 0, Knowledge.data_numGhostLayers, 1, ((Knowledge.domain_fragLengthPerDim(dim) * (1 << level.get.asInstanceOf[SingleLevelSpecification].level)) + 1) - 2 /*dup*/ , 1, Knowledge.data_numGhostLayers, 0)) ++
          (Knowledge.dimensionality until 3).toArray.map(dim => new FieldLayoutPerDim(0, 0, 0, 1, 0, 0, 0))
  	new Field(
  	    name, 
  	    0, // FIXME: domain
  	    name.substring(0, 3).toLowerCase + "Data", // HACK
  	    "double", // FIXME: datatype,
  	    layout, // FIXME: get this info from the DSL
  	    level.get.asInstanceOf[SingleLevelSpecification].level,
  	    slots,
  	    new MultiIndex(layout.map(l => l.idxDupLeftBegin)), // FIXME: offset
  	    bcDir0);
  }
}
