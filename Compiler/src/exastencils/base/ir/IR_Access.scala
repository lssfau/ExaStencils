package exastencils.base.ir

import exastencils.communication.ir.IR_IV_AbstractCommBuffer
import exastencils.polyhedron.IR_PolyArrayAccessLike
import exastencils.prettyprinting._

/// IR_Access

trait IR_Access extends IR_Expression {
  // FIXME: def name : String
}

/// IR_VariableAccess

object IR_VariableAccess {
  def apply(decl : IR_VariableDeclaration) = new IR_VariableAccess(decl.name, decl.datatype)
}

case class IR_VariableAccess(var name : String, var datatype : IR_Datatype) extends IR_Access {
  override def prettyprint(out : PpStream) : Unit = out << name
}

/// IR_ArrayAccess

// TODO: split into multidimensional (IR_MultiDimArrayAccess) and linear
case class IR_ArrayAccess(var base : IR_Expression, var index : IR_Expression, var alignedAccessPossible : Boolean = false) extends IR_Access with IR_PolyArrayAccessLike {
  // TODO: shouldn't this be a deref?
  override def datatype = base.datatype

  override def prettyprint(out : PpStream) : Unit = {
    index match {
      case ind : IR_Index      => out << base << ind
      case ind : IR_Expression => out << base << '[' << ind << ']'
    }
  }

  override def uniqueID : String = {
    base match {
      case IR_StringLiteral(name)          => name
      case IR_VariableAccess(name, _)      => name
      case buff : IR_IV_AbstractCommBuffer => buff.prettyprint()
      case _                               => null
    }
  }
}

case class IR_HighDimAccess(var base : IR_Expression, var index : IR_Index) extends IR_Access {
  // TODO: modify this to use IR_HighDimIndex

  // Access to matrices, needs to be linearized before prettyprinting
  override def datatype = base.datatype.resolveDeclType

  override def prettyprint(out : PpStream) : Unit = {
    val expIdx = index.toExpressionIndex
    out << '(' << base
    expIdx.prettyprint(out)
    out << ')'
  }
}

/// IR_MultiDimArrayAccess

// non-linearized multi dimensional access to arrays of pointers (to pointers, ...) to data, e.g. a[z-1][y+1][x]
case class IR_MultiDimArrayAccess(var base : IR_Expression, var index : IR_ExpressionIndex) extends IR_Access {
  override def datatype = base.datatype

  override def prettyprint(out : PpStream) : Unit = {
    out << base
    index.indices.reverse.foreach { ix =>
      out << '[' << ix << ']'
    }
  }

  def expandSpecial = {
    var wrapped : IR_Expression = base
    for (i <- index.indices.reverse) // TODO: reverse or not?
      wrapped = IR_ArrayAccess(wrapped, i)
    wrapped
  }
}

/// IR_DerefAccess

case class IR_DerefAccess(var base : IR_Access) extends IR_Access {
  // FIXME: deref datatype
  override def datatype = base.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(*(" << base << "))"
}
