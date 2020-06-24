package exastencils.baseExt.ir

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.prettyprinting.PpStream

object IR_MatStructure {
  def apply(s : String, b : Int, sA : String, bA : Int) = {
    new IR_MatStructure(s, b, sA, bA)
  }
}
//TODO extends IR_Expression?
case class IR_MatStructure(var structure : String, var blocksize : Int, var structureA : String, var blocksizeA : Int) extends IR_Expression {
  override def datatype : IR_Datatype = ???
  override def prettyprint(out : PpStream) : Unit = ???
}

