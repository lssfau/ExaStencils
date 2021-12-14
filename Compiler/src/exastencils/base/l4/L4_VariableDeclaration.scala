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

package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.baseExt.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_VariableDeclaration

case class L4_VariableDeclaration(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var datatype : L4_Datatype,
    var initialValue : Option[L4_Expression],
    var isConst : Boolean) extends L4_Statement {

  if (initialValue.isDefined) {
    // TODO: extract to separate strategy
    (datatype, initialValue.get) match { // FIXME does not work if initialValue is nested, e.g. inverse(L4_MatrixExpression)
      case (dt : L4_VectorDatatype, exp : L4_VectorExpression) =>
        initialValue.get.asInstanceOf[L4_VectorExpression].datatype = Some(dt.datatype)
        initialValue.get.asInstanceOf[L4_VectorExpression].convertConstants(dt.datatype)
        if (dt.isRow != exp.rowVector) Logger.error(s"Mismatch in assignment for Variable $name: incompatible vector types")

      case (dt : L4_MatrixDatatype, _ : L4_MatrixExpression) =>
        initialValue.get.asInstanceOf[L4_MatrixExpression].datatype = Some(dt.datatype)
        initialValue.get.asInstanceOf[L4_MatrixExpression].convertConstants(dt.datatype)

      case (dt : L4_TensorDatatype1, _ : L4_TensorExpression1) =>
        initialValue.get.asInstanceOf[L4_TensorExpression1].datatype = Some(dt.datatype)
        initialValue.get.asInstanceOf[L4_TensorExpression1].convertConstants(dt.datatype)

      case (dt : L4_TensorDatatype2, _ : L4_TensorExpression2) =>
        initialValue.get.asInstanceOf[L4_TensorExpression2].datatype = Some(dt.datatype)
        initialValue.get.asInstanceOf[L4_TensorExpression2].convertConstants(dt.datatype)

      case (dt : L4_TensorDatatypeN, _ : L4_TensorExpressionN) =>
        initialValue.get.asInstanceOf[L4_TensorExpressionN].datatype = Some(dt.datatype)
        initialValue.get.asInstanceOf[L4_TensorExpressionN].convertConstants(dt.datatype)

      case _ =>
    }
  }

  override def prettyprint(out : PpStream) = {
    out << (if (isConst) "Val " else "Var ") << name
    if (levels.isDefined) out << '@' << levels.get
    out << " : " << datatype
    if (initialValue.isDefined) out << " = " << initialValue.get
  }

  def unfold = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled variable/ value declaration for $name")

    val levelList = L4_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L4_SingleLevel(level))
      newDecl
    })
  }

  override def progress = ProgressLocation {
    val newName = if (levels.isDefined) name + "_" + levels.get.resolveLevel else name
    IR_VariableDeclaration(datatype.progress, newName, L4_ProgressOption(initialValue)(_.progress), isConst)
  }
}

/// L4_UnfoldLeveledVariableDeclarations

object L4_UnfoldLeveledVariableDeclarations extends DefaultStrategy("Unfold leveled value and variable declarations") {
  this += new Transformation("Unfold", {
    case decl : L4_VariableDeclaration if decl.levels.isDefined => decl.unfold
  })
}
