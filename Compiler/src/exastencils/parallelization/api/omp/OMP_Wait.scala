package exastencils.parallelization.api.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._

case object OMP_WaitForFlag extends IR_FuturePlainFunction {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  override var name = "waitForFlag"
  override def prettyprint_decl() : String = prettyprint

  def flag = IR_VariableAccess("flag", IR_PointerDatatype(IR_VolatileDatatype(IR_BooleanDatatype)))

  override def generateFct() = {
    val fct = IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(flag), ListBuffer[IR_Statement]())
    fct.allowInlining = false

    // add busy waiting loop
    fct.body += IR_WhileLoop(IR_Negation(IR_DerefAccess(flag)), ListBuffer[IR_Statement]())
    fct.body += IR_Assignment(IR_DerefAccess(flag), IR_BooleanConstant(false))

    fct
  }
}
