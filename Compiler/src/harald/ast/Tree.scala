package harald.ast

import scala.collection.mutable.ListBuffer
import harald.Abstract._
import harald.Impl._
import harald.dsl._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class TreeL2() extends Node {
  val exaFields: ListBuffer[AbstractField] = new ListBuffer()
  val exaOperators: ListBuffer[AbstractStencil] = new ListBuffer()

  val Fields: ListBuffer[ImplField] = new ListBuffer()
  val GhostFields: ListBuffer[ImplField] = new ListBuffer()
  val Stencils: ListBuffer[ImplStencil] = new ListBuffer()

  def isinFields(s : String) : Boolean = {
    for (f <- Fields)
      if (f.name.equals(s))
        return true
    return false    
  }
  
   // transformations
  def transformStencils {
    Stencils.clear
    for (e <- exaOperators)
      Stencils ++= e.transform
  }

  def transformFields {
    Fields.clear
    for (e <- exaFields)
      Fields ++= e.transform
  }

  val exaFunctions: ListBuffer[AbstractFunction] = new ListBuffer()
  val exaClasses: ListBuffer[AbstractClass] = new ListBuffer()
  val exaDefinitions: ListBuffer[AbstractDefinition] = new ListBuffer()

  var GlobalDefines: collection.mutable.Map[String, ParameterInfo] = collection.mutable.Map()
}