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

package exastencils.layoutTransformation.l4

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_MatrixDatatype
import exastencils.baseExt.l4.L4_TensorDatatype1
import exastencils.baseExt.l4.L4_TensorDatatype2
import exastencils.baseExt.l4.L4_TensorDatatypeN
import exastencils.baseExt.l4.L4_VectorDatatype
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l4.L4_BaseFieldDecl
import exastencils.field.l4.L4_FieldLayoutDecl
import exastencils.layoutTransformation.ir.IR_LayoutTransformationCollection
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_LayoutSection

case class L4_LayoutSection(var statements : ListBuffer[L4_LayoutTransformStatement] = ListBuffer()) extends L4_Node with PrettyPrintable with L4_Progressable {
  override def prettyprint(out : PpStream) : Unit = {
    if (statements.nonEmpty) out << "LayoutTransformations {\n" <<< (statements, "\n") << "\n}"
  }

  override def progress = ProgressLocation(IR_LayoutTransformationCollection(statements.map(_.progress)))
}

/// L4_AaddSOAtoAOSTransformation

object L4_AddSoAtoAoSTransformation extends DefaultStrategy("Add SoA to AoS transformation for all fields with matrix or vector element type") {

  override def apply(applyAtNode : Option[Node]) : Unit = {
    val fieldDecls = ArrayBuffer[L4_BaseFieldDecl]()
    val layoutDecls = ArrayBuffer[L4_FieldLayoutDecl]()
    var break = false

    this += new Transformation("collect fields with matrix or vector elements", {
      case n if break                 => // early exit as the transformation is not recursive
        n
      case fDecl : L4_BaseFieldDecl   =>
        fieldDecls += fDecl
        fDecl
      case lDecl : L4_FieldLayoutDecl =>
        lDecl.datatype match {
          case _ : L4_VectorDatatype | _ : L4_MatrixDatatype | _ : L4_TensorDatatype1 | _ : L4_TensorDatatype2 | _ : L4_TensorDatatypeN =>
            layoutDecls += lDecl
          case _                                                                      =>
        }
        lDecl
      case lSec : L4_LayoutSection    =>
        break = true
        lSec
    }, recursive = false)
    super.apply(applyAtNode)

    if (break) {
      Logger.warn("No layout transformations are created since there already exists at least one LayoutTransformations block")
      return
    }

    val layouts = HashMap[String, HashMap[L4_LevelSpecification, (ArrayBuffer[L4_LeveledIdentifier], L4_GenericTransform)]]()
    for (L4_FieldLayoutDecl(name, levels, dt, _, _) <- layoutDecls) {
      val dim : Int = dt.dimensionality
      val fieldIts = (0 until Knowledge.dimensionality).view.map(i => L4_PlainVariableAccess("i" + i, L4_IntegerDatatype, true))
      val matIts = dim match {
        case 1 => List(L4_PlainVariableAccess("v", L4_IntegerDatatype, true)).view
        case d => (0 until d).view.map(i => L4_PlainVariableAccess("m" + i, L4_IntegerDatatype, true))
      }
      val its = (fieldIts ++ matIts).toArray
      val trafo = (matIts ++ fieldIts).toArray[L4_Expression]
      val map = layouts.getOrElseUpdate(name, HashMap())
      val buffer = ArrayBuffer[L4_LeveledIdentifier]()
      map.put(levels.getOrElse(L4_AllLevels), (buffer, L4_GenericTransform(buffer, its, L4_ExpressionIndex(trafo))))
    }

    for (L4_BaseFieldDecl(name, levelsOpt, _, layout, _, _, _) <- fieldDecls)
      for (layoutLevels <- layouts.get(layout.name)) { // option
        val levels = levelsOpt.getOrElse(L4_AllLevels)
        val trafo = layoutLevels.getOrElse(levels, layoutLevels.getOrElse(L4_AllLevels, null))
        if (trafo != null)
          trafo._1 += L4_LeveledIdentifier(name, Some(Duplicate(levels)))
        else
          Logger.warn(s"creating a default SoA to AoS layout transformation for field $name@${ levels.prettyprint() } not possible " +
            s"(currently the level flag for the layout must either be '@all' or exactly match the one from the field declaration: ${ levels.prettyprint() })")
      }

    val layoutSec = L4_LayoutSection()
    for ((_, map) <- layouts)
      for ((_, (_, trafoStmt)) <- map)
        if (!trafoStmt.fields.isEmpty)
          layoutSec.statements += trafoStmt

    val nodes = ExaRootNode.l4_root.nodes
    nodes.insert(nodes.lastIndexWhere(_.isInstanceOf[L4_BaseFieldDecl]) + 1, layoutSec)
  }
}
