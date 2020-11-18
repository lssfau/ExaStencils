package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.field.ir._

case class IR_FileAccess_SionLib(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var includeGhostLayers : Boolean,
    var writeAccess : Boolean,
    var appendedMode : Boolean = false) extends IR_FileAccess(filename, field, slot, includeGhostLayers, writeAccess, appendedMode) {

  // TODO
  override def createOrOpenFile() : ListBuffer[IR_Statement] = ???
  override def setupAccess() : ListBuffer[IR_Statement] = ???
  override def cleanupAccess() : ListBuffer[IR_Statement] = ???
  override def closeFile() : ListBuffer[IR_Statement] = ???
  override def openMode : IR_VariableAccess = ???
  override def accessFileFragwise(accessStatements : ListBuffer[IR_Statement]) : IR_LoopOverFragments = ???
  override def readField() : ListBuffer[IR_Statement] = ???
  override def writeField() : ListBuffer[IR_Statement] = ???

  // TODO
  override def includes : ListBuffer[String] = super.includes
  override def libraries : ListBuffer[String] = super.libraries
  override def pathsInc : ListBuffer[String] = super.pathsInc
  override def pathsLib : ListBuffer[String] = super.pathsLib
}
