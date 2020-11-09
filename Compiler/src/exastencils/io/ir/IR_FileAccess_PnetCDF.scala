package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.field.ir._
import exastencils.logger.Logger

case class IR_FileAccess_PnetCDF(
    var fileName : IR_Expression,
    var datasetName : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var includeGhostLayers : Boolean,
    var appendedMode : Boolean = false,
    var writeAccess : Boolean) extends IR_FileAccess(fileName, field, slot, includeGhostLayers, writeAccess, appendedMode) {

  override def prologue() : ListBuffer[IR_Statement] = ???
  override def epilogue() : ListBuffer[IR_Statement] = ???

  override def readField() : ListBuffer[IR_Statement] = ???
  override def writeField() : ListBuffer[IR_Statement] = ???

  override def expand() : Output[StatementList]  = {
    // TODO: PnetCDF headers

    // TODO: differences/similarities of read/write operations

    if(datasetName == IR_NullExpression) {
      Logger.error("Parameter \"dataset\" was not specified.")
    }

    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts ++= prologue()
    stmts ++= kernel()
    stmts ++= epilogue()
    stmts
  }
}
