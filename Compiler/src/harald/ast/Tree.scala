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

  val Stencils: ListBuffer[ImplStencil] = new ListBuffer()

   // transformations
  def transformStencils {
    Stencils.clear
    for (e <- exaOperators)
      Stencils ++= e.transform
  }

  val exaFunctions: ListBuffer[AbstractFunction] = new ListBuffer()
  val exaClasses: ListBuffer[AbstractClass] = new ListBuffer()
  val exaDefinitions: ListBuffer[AbstractDefinition] = new ListBuffer()
}