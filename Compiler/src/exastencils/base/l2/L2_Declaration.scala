package exastencils.base.l2

import exastencils.base.l3._
import exastencils.prettyprinting.PpStream

/// L2_ValueDeclaration

case class L2_ValueDeclaration(
    var name : String,
    var datatype : L2_Datatype,
    var initialValue : L2_Expression) extends L2_Statement {

  override def prettyprint(out : PpStream) = out << "Value " << name << " : " << datatype << " = " << initialValue
  override def progress = L3_ValueDeclaration(name, datatype.progress, initialValue.progress)
}

/// L2_VariableDeclaration

case class L2_VariableDeclaration(
    var name : String,
    var datatype : L2_Datatype,
    var initialValue : Option[L2_Expression] = None) extends L2_Statement {

  override def prettyprint(out : PpStream) = {
    out << "Variable " << name << " : " << datatype
    if (initialValue.isDefined) out << " = " << initialValue.get
  }

  override def progress = L3_VariableDeclaration(name, datatype.progress, L2_ProgressOption(initialValue)(_.progress))
}
