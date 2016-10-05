package exastencils.omp.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.{ IR_Function, _ }
import exastencils.datastructures.Transformation.Output
import exastencils.prettyprinting.PpStream

case object OMP_WaitForFlag extends IR_AbstractFunction with IR_Expandable {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() : String = prettyprint
  override def name = "waitForFlag"

  def flag = IR_VariableAccess("flag", IR_PointerDatatype(IR_VolatileDatatype(IR_BooleanDatatype)))

  override def expand() : Output[IR_Function] = {
    val fct = IR_Function(IR_UnitDatatype, name, IR_FunctionArgument(flag), ListBuffer[IR_Statement]())
    fct.allowInlining = false

    // add busy waiting loop
    fct.body += IR_WhileLoop(IR_NegationExpression(IR_DerefAccess(flag)), ListBuffer[IR_Statement]())
    fct.body += IR_Assignment(IR_DerefAccess(flag), IR_BooleanConstant(false))

    fct
  }
}
