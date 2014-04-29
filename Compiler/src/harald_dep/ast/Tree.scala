package harald_dep.ast

import scala.collection.mutable.ListBuffer
import harald_dep.Abstract._
import harald_dep.Impl._
import harald_dep.dsl._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

case class TreeL2() extends Node {
  val exaFields: ListBuffer[AbstractField] = new ListBuffer()
  val exaOperators: ListBuffer[AbstractStencil] = new ListBuffer()
  val exaFunctions: ListBuffer[AbstractFunction] = new ListBuffer()
  val exaClasses: ListBuffer[AbstractClass] = new ListBuffer()
  val exaDefinitions: ListBuffer[AbstractDefinition] = new ListBuffer()
}