package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.prettyprinting.PpStream

trait IR_WaLBerlaBlockLoopVariable extends IR_Expression {
  def resolveName() : String
  def resolveDatatype() : IR_Datatype

  def resolveAccess() : IR_VariableAccess = IR_VariableAccess(resolveName(), resolveDatatype())

  def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName())

  override def datatype = resolveDatatype()
  override def prettyprint(out : PpStream) : Unit = out << resolveName
}

object IR_WaLBerlaFindBlockLoopVariables extends QuietDefaultStrategy("Find accesses with refinement") {
  var blockLoopVariables : mutable.HashSet[IR_WaLBerlaBlockLoopVariable] = mutable.HashSet()

  override def applyStandalone(node : Node) : Unit = {
    blockLoopVariables = mutable.HashSet()
    super.applyStandalone(node)
  }

  this += Transformation("..", {
    case acc : IR_WaLBerlaBlockLoopVariable =>
      blockLoopVariables += acc
      acc
  })
}
