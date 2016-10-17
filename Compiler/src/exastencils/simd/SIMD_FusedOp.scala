package exastencils.simd

import exastencils.base.ir.IR_Expression
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// SIMD_MultiplyAdd

case class SIMD_MultiplyAdd(
    var factor1 : IR_Expression,
    var factor2 : IR_Expression,
    var summand : IR_Expression) extends SIMD_Expression {
  override def datatype = IR_ResultingDatatype(factor1.datatype, factor2.datatype, summand.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    SIMD_FusedPrinterHelper.prettyprint(out, factor1, factor2, summand, "add")
  }
}

/// SIMD_MultiplySub

case class SIMD_MultiplySub(
    var factor1 : IR_Expression,
    var factor2 : IR_Expression,
    var summand : IR_Expression) extends SIMD_Expression {
  override def datatype = IR_ResultingDatatype(factor1.datatype, factor2.datatype, summand.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    SIMD_FusedPrinterHelper.prettyprint(out, factor1, factor2, summand, "sub")
  }
}
