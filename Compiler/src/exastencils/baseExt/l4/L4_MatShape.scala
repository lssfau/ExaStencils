package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Expression
import exastencils.baseExt.ir.IR_MatShape
import exastencils.prettyprinting.PpStream

object L4_MatShape  {
  def apply(structure : String) =
    new L4_MatShape(structure)
}
case class L4_MatShape(
    shape : String,
    var sizes : Option[ListBuffer[(String,Int)]] = None,
    var substructures : Option[ListBuffer[(String,String)]] = None
) extends L4_Expression {
  override def progress : IR_MatShape = ProgressLocation(IR_MatShape(shape))
  override def prettyprint(out : PpStream) : Unit = ???
}
