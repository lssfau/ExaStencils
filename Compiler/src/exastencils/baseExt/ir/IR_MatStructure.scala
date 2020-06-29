package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_StringConstant
import exastencils.prettyprinting.PpStream

object IR_MatStructure {
  def apply(structure : String, blocksize : Int = -1, structureA : String = "-1", blocksizeA : Int = -1) = {
    new IR_MatStructure(structure, blocksize, structureA, blocksizeA)
  }
}
case class IR_MatStructure(var structure : String, var blocksize : Int, var structureA : String, var blocksizeA : Int) extends IR_Expression {
  override def datatype : IR_Datatype = ???
  override def prettyprint(out : PpStream) : Unit = ???
  def toList() : ListBuffer[IR_Expression] = {
    ListBuffer[IR_Expression](IR_StringConstant(structure), IR_IntegerConstant(blocksize), IR_StringConstant(structureA), IR_IntegerConstant(blocksizeA))
  }
}

