package exastencils.base.ir

import exastencils.prettyprinting._

trait IR_Access extends IR_Expression {
  // TODO: def name : String
}

/// IR_VariableAccess

object IR_VariableAccess {
  def apply(name : String) = new IR_VariableAccess(name, None)
  def apply(name : String, datatype : IR_Datatype) = new IR_VariableAccess(name, Some(datatype))
  def apply(decl : IR_VariableDeclaration) = new IR_VariableAccess(decl.name, Some(decl.datatype))
}

// FIXME: mandatory datatype
case class IR_VariableAccess(var name : String, var innerDatatype : Option[IR_Datatype]) extends IR_Access {
  override def datatype = innerDatatype.getOrElse(IR_RealDatatype)
  override def prettyprint(out : PpStream) : Unit = out << name
}

/// IR_ArrayAccess

// TODO: split into multidimensional and linear
case class IR_ArrayAccess(var base : IR_Expression, var index : IR_Expression, var alignedAccessPossible : Boolean = false) extends IR_Access {
  // TODO: shouldn't this be a deref?
  override def datatype = base.datatype
  override def prettyprint(out : PpStream) : Unit = {
    index match {
      case ind : IR_Index      => out << base << ind
      case ind : IR_Expression => out << base << '[' << ind << ']'
    }
  }
}
