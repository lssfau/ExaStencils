package exastencils.parallelization.api.mpi

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output

/// MPI_GeneratedTag

case class MPI_GeneratedTag(var from : IR_Expression, var to : IR_Expression, var dirOfSend : IR_Expression, var concurrencyId : Int) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  def expand() : Output[IR_Expression] = {
    // ("((unsigned int)" ~ from ~ " << 20)") + ("((unsigned int)(" ~ to ~ ") << 10)") + concurrencyId
    //CastExpression(SpecialDatatype("unsigned int"), from << IntegerConstant(20)) + CastExpression(SpecialDatatype("unsigned int"), to << IntegerConstant(10)) + concurrencyId
    (IR_Cast(IR_SpecialDatatype("unsigned int"), from << IR_IntegerConstant(31 - 8))
      + IR_Cast(IR_SpecialDatatype("unsigned int"), to << IR_IntegerConstant(31 - 16))
      + IR_Cast(IR_SpecialDatatype("unsigned int"), dirOfSend << IR_IntegerConstant(31 - 21))
      + concurrencyId)
  }
}
