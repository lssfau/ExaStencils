package exastencils.datastructures.l4

import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.multiGrid.MultiGrid

case class Root(var fields : List[FieldDeclarationStatement], var iterationSets : List[IterationSetDeclarationStatement], var statements : List[Statement]) extends Node with ProgressableToIr {
  def this(statements : List[Statement]) = this(List(), List(), statements)

  def getFieldByIdentifier(identifier : String, level : Int) : Option[FieldDeclarationStatement] = {
    fields.find(f => f.name == identifier && f.level.getOrElse(-1) == level)
  }

  def progressToIr : Node = {
    var newRoot = new ir.Root

    var fieldCollection = new FieldCollection
    for (field <- fields)
      fieldCollection.fields += field.progressToIr
    newRoot += fieldCollection

    var iterationSetCollection = new IterationSetCollection
    for (iterationSet <- iterationSets)
      iterationSetCollection.sets += iterationSet.progressToIr
      newRoot += iterationSetCollection
    
    var multiGrid = new MultiGrid // FIXME: think about how to manage (MG/other) functions
    for (node <- statements)
      node match {
        case function : FunctionStatement => multiGrid.functions_HACK += function.progressToIr
      }
    newRoot += multiGrid

    newRoot
  }
}
