//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.base.ir

import java.lang.System.Logger

import exastencils.communication.ir.IR_IV_AbstractCommBuffer
import exastencils.polyhedron.IR_PolyArrayAccessLike
import exastencils.prettyprinting._

/// IR_Access

trait IR_Access extends IR_Expression {
  // FIXME: def name : String
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

case class IR_HighDimAccess(var base : IR_Expression, var index : IR_Index) extends IR_Access with IR_PolyArrayAccessLike {
  // TODO: modify this to use IR_HighDimIndex

  // Access to matrices, needs to be linearized before prettyprinting
  override def datatype = base.datatype.resolveDeclType

  override def prettyprint(out : PpStream) : Unit = {
    val expIdx = index.toExpressionIndex
    out << '(' << base
    expIdx.prettyprint(out)
    out << ')'
  }

  override def uniqueID : String = {
    base match {
      case IR_StringLiteral(name)     => name
      case IR_VariableAccess(name, _) => name
      case _                          => null
    }
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
