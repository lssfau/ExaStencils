package exastencils.solver.ir

import exastencils.base.ir._
import exastencils.knowledge.ir.IR_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream

/// IR_EquationAccess

case class IR_EquationAccess(
    var target : IR_NamedEquation,
    var offset : Option[IR_ConstIndex] = None) extends IR_LeveledKnowledgeAccess with IR_CanBeOffset {

  override def datatype = IR_UnknownDatatype

  def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

}
