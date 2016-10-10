package exastencils.interfacing.ir

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config._
import exastencils.datastructures._
import exastencils.prettyprinting.PpStream

/// IR_ExternalFieldAccess

case class IR_ExternalFieldAccess(var name : IR_Expression, var field : IR_ExternalField, var index : IR_ExpressionIndex) extends IR_Expression {
  // TODO: var index : IR_Index
  override def datatype = field.fieldLayout.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  val alignedAccessPossible = false

  def linearize : IR_ArrayAccess = {
    if (Knowledge.generateFortranInterface) {
      // Fortran requires multi-index access to multidimensional arrays
      val it = IR_LoopOverDimensions.defIt(field.fieldLayout.numDimsData)
      var ret = name
      for (dim <- field.fieldLayout.numDimsData - 1 to 0)
        ret = IR_ArrayAccess(ret, it(dim), alignedAccessPossible)
      ret.asInstanceOf[IR_ArrayAccess]
    } else
      IR_ArrayAccess(name, field.fieldLayout.linearizeIndex(index), alignedAccessPossible)
  }
}

/// IR_LinearizeExternalFieldAccess

object IR_LinearizeExternalFieldAccess extends DefaultStrategy("Linearize ExternalFieldAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_ExternalFieldAccess => access.linearize
  })
}

