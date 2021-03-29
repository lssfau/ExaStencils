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

package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_Expression
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_MatrixAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4.L4_FieldCollection
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_VariableDeclarationCollector

/// L4_MatrixAccess
object L4_MatrixAccess {
  def apply(acc : L4_Access, idxy : L4_Index, idxx : Option[L4_Index]) : L4_MatrixAccess = {
    if (idxx.isDefined) idxx.get match {
      case expridx : L4_ExpressionIndex =>
        if (expridx.indices.length > 1) Logger.error(s"Matrix access with more than 1 indices not allowed")
      case cidx : L4_ConstIndex         =>
        if (cidx.indices.length > 1) Logger.error(s"Matrix access with more than 1 indices not allowed")
      case ridx : L4_RangeIndex         =>
      case _                            => Logger.error("unexprected index type")
    }
    idxy match {
      case expridx : L4_ExpressionIndex =>
        if (expridx.indices.length > 1) Logger.error(s"Matrix access with more than 1 indices not allowed")
      case cidx : L4_ConstIndex         =>
        if (cidx.indices.length > 1) Logger.error(s"Matrix access with more than 1 indices not allowed")
      case ridx : L4_RangeIndex         =>
      case _                            => Logger.error("unexprected index type")
    }
    new L4_MatrixAccess(acc, idxy, idxx)
  }
}

case class L4_MatrixAccess(acc : L4_Access, idxy : L4_Index, idxx : Option[L4_Index]) extends L4_Access {
  override def progress : IR_Expression = ProgressLocation(IR_MatrixAccess(acc.progress, idxy.progress, if (idxx.isDefined) Some(idxx.get.progress) else None))
  override def prettyprint(out : PpStream) : Unit = {
    out << name << idxy << idxx
  }
  override def name : String = acc.name
}

object L4_PrepareMatrixAccesses extends DefaultStrategy("Prepare matrix accesses") {
  var declCollector = new L4_VariableDeclarationCollector
  this.register(declCollector)

  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Prepare", {
    case uacc : L4_UnresolvedAccess if (uacc.matIndex.isDefined) =>
      val decl = declCollector.plainDeclarations.last.get(uacc.name)
      val fieldFound = L4_FieldCollection.exists(uacc.name)
      if (decl.isEmpty && !fieldFound) Logger.error("Declaration for access not found")
      else if (!decl.get.datatype.isInstanceOf[L4_MatrixDatatype]) Logger.error("Access with matIndex to non matrix variable")
      // get mat field accesses in ir
      if (fieldFound) uacc
      else {
        if (uacc.level.isDefined) Logger.warn("Discarding level on variable access to matrix variable")
        if (uacc.slot.isDefined) Logger.warn("Discarding slot on variable access to matrix variable")
        if (uacc.arrayIndex.isDefined) Logger.warn("Discarding array index on variable access to matrix variable")
        if (uacc.offset.isDefined) Logger.warn("Discarding offset on variable access to matrix variable")
        if (uacc.dirAccess.isDefined) Logger.warn("Discarding dirAccess on variable access to matrix variable")
        L4_MatrixAccess(L4_PlainVariableAccess(uacc.name, decl.get.datatype, false), uacc.matIndex.get(0), if (uacc.matIndex.get.length == 2) Some(uacc.matIndex.get(1)) else None)
      }
  })

}

