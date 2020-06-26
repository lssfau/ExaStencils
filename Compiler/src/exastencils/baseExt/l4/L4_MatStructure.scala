package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Expression
import exastencils.baseExt.ir.IR_MatStructure
import exastencils.prettyprinting.PpStream

object L4_MatStructure  {
  def apply(structure : String, blocksize: Int, structureA : String, blocksizeA : Int) =
    new L4_MatStructure(structure, blocksize, structureA, blocksizeA)
}
case class L4_MatStructure(structure : String, blocksize: Int, structureA : String, blocksizeA : Int) extends L4_Expression {
  override def progress : IR_MatStructure = ProgressLocation(IR_MatStructure(structure, blocksize, structureA, blocksizeA))
  override def prettyprint(out : PpStream) : Unit = ???
}
