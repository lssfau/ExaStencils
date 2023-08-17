package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

trait IR_ProcessLocalBlockLoopVariable extends IR_Access {
  def resolveName() : String
  def resolveDatatype() : IR_Datatype

  def resolveAccess() : IR_VariableAccess = IR_VariableAccess(resolveName(), resolveDatatype())

  def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName())

  override def datatype = resolveDatatype()
  override def prettyprint(out : PpStream) : Unit = out << resolveName
}
