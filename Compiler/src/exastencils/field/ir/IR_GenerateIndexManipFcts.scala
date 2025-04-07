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

package exastencils.field.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._

/// IR_GenerateIndexManipFcts

object IR_GenerateIndexManipFcts extends DefaultStrategy("Generate index manipulation functions") {
  // collect indices -> per layout/level
  // add function with
  //  get old_inner IE-IB
  //  calculate offset as new_inner - old_inner
  //  adapt IE,GRB,GRE,etc with +offset

  var layoutMap = mutable.HashMap[String, (String, IR_Expression)]()

  override def apply(node : Option[Node] = None) = {
    layoutMap.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    layoutMap.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collect", {
    case idx : IR_IV_IndexFromField =>
      layoutMap += (s"${ idx.layoutIdentifier }_${ idx.level.prettyprint }" -> (idx.layoutIdentifier, idx.level))
      idx
  })

  this += new Transformation("Generate functions", {
    case functions : IR_UserFunctions =>
      for (layout <- layoutMap) {
        var body = ListBuffer[IR_Statement]()
        def newInnerSize(dim : Integer) = IR_VariableAccess(s"newInnerSize_$dim", IR_IntegerDatatype)
        def idxShift(dim : Integer) = IR_VariableAccess(s"idxShift_$dim", IR_IntegerDatatype)

        // compile body for all dimensions - TODO: adapt to field layout dimensionality if required
        for (dim <- 0 until Knowledge.dimensionality) {
          // calculate index shift
          body += IR_VariableDeclaration(idxShift(dim), newInnerSize(dim) - (
            IR_IV_IndexFromField(layout._2._1, layout._2._2, "IE", dim) -
              IR_IV_IndexFromField(layout._2._1, layout._2._2, "IB", dim)))

          // adapt indices
          for (idxIdent <- List("IE", "DRB", "DRE", "GRB", "GRE", "PRB", "PRE", "TOT")) {
            body += IR_Assignment(
              IR_IV_IndexFromField(layout._2._1, layout._2._2, idxIdent, dim),
              idxShift(dim), "+=")
          }
        }

        // wrap body in fragment loop
        body = ListBuffer[IR_Statement](IR_LoopOverFragments(body))

        // set up function
        functions += IR_PlainFunction( /* FIXME: IR_LeveledFunction -> level as Int */
          s"resizeInner_${ layout._2._1 }_${ layout._2._2.prettyprint }",
          IR_UnitDatatype,
          Knowledge.dimensions.map(dim => IR_FunctionArgument(newInnerSize(dim))).to[ListBuffer],
          body).withNoInline
      }

      // generate a special resize functions for all fields on a given level
      for (level <- Knowledge.maxLevel to Knowledge.minLevel by -1) {
        var body = ListBuffer[IR_Statement]()
        def newInnerSize(dim : Integer) = IR_VariableAccess(s"newInnerSize_$dim", IR_IntegerDatatype)

        // generate function calls with adapted sizes
        for (layout <- layoutMap.filter(level == _._2._2.prettyprint.toInt).toSeq.sortBy(_._1)) {
          body += IR_FunctionCall(IR_LeveledInternalFunctionReference(s"resizeInner_${ layout._2._1 }", level, IR_UnitDatatype),
            Knowledge.dimensions.map(dim => newInnerSize(dim) : IR_Expression).to[ListBuffer])
        }

        // for empty function bodies: mark function parameters as unused
        if (body.isEmpty)
          body ++= Knowledge.dimensions.map(dim => IR_ExpressionStatement(IR_Cast(IR_UnitDatatype, newInnerSize(dim))))

        // set up function
        functions += IR_LeveledFunction(
          "resizeAllInner", level, IR_UnitDatatype,
          Knowledge.dimensions.map(dim => IR_FunctionArgument(newInnerSize(dim))).to[ListBuffer],
          body).withNoInline
      }

      // return extended collection
      functions
  })
}
