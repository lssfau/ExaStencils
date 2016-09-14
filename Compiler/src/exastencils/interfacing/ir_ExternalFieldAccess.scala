package exastencils.interfacing

import exastencils.base.ir._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream

case class IR_ExternalFieldAccess(var name : IR_Expression, var field : ExternalField, var index : IR_ExpressionIndex) extends IR_Expression {
  // TODO: var index : IR_Index
  override def datatype = field.fieldLayout.datatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ExternalFieldAccess\n"

  val alignedAccessPossible = false

  def linearize : ArrayAccess = {
    if (Knowledge.generateFortranInterface) {
      // Fortran requires multi-index access to multidimensional arrays
      val it = LoopOverDimensions.defIt(field.fieldLayout.numDimsData)
      var ret = name
      for (dim <- field.fieldLayout.numDimsData - 1 to 0)
        ret = ArrayAccess(ret, it(dim), alignedAccessPossible)
      ret.asInstanceOf[ArrayAccess]
    } else
      ArrayAccess(name, Mapping.resolveMultiIdx(field.fieldLayout, index), alignedAccessPossible)
  }
}
