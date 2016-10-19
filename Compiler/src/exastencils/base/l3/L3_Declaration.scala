package exastencils.base.l3

import exastencils.base.l4._
import exastencils.prettyprinting.PpStream

/// L3_ValueDeclaration

case class L3_ValueDeclaration(
    var name : String,
    var datatype : L3_Datatype,
    var initialValue : L3_Expression) extends L3_Statement {

  override def prettyprint(out : PpStream) = out << "Value " << name << " : " << datatype << " = " << initialValue
  override def progress = L4_ValueDeclaration(L4_BasicIdentifier(name), datatype.progress, initialValue.progress)
}

/// L3_VariableDeclaration

case class L3_VariableDeclaration(
    var name : String,
    var datatype : L3_Datatype,
    var initialValue : Option[L3_Expression] = None) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    out << "Variable " << name << " : " << datatype
    if (initialValue.isDefined) out << " = " << initialValue.get
  }

  override def progress = L4_VariableDeclaration(L4_BasicIdentifier(name), datatype.progress, L3_ProgressOption(initialValue)(_.progress))
}
