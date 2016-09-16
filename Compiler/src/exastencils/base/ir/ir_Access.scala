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

// TODO: split into multidimensional (IR_MultiDimArrayAccess) and linear
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

/// IR_MultiDimArrayAccess

// non-linearized multi dimensional access to arrays of pointers (to pointers, ...) to data, e.g. a[z-1][y+1][x]
case class IR_MultiDimArrayAccess(var base : IR_Expression, var index : IR_ExpressionIndex) extends IR_Access {
  // FIXME how to get the base data type of an array expression?
  override def datatype = ???
  override def prettyprint(out : PpStream) : Unit = {
    out << base
    index.foreach({ ix =>
      out << '[' << ix << ']'
    })
  }

  def expandSpecial = {
    var wrapped : IR_Expression = base
    for (i <- index.indices.reverse) // TODO: reverse or not?
      wrapped = IR_ArrayAccess(wrapped, i)
    wrapped
  }
}
