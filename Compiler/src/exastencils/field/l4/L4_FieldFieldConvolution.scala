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

package exastencils.field.l4

import scala.collection.mutable._

import exastencils.base.ir.IR_Expression
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_LoopOverField
import exastencils.communication.l4.L4_Communicate
import exastencils.core._
import exastencils.datastructures._
import exastencils.prettyprinting.PpStream

/// L4_FieldFieldConvolution

case class L4_FieldFieldConvolution(var lhs : L4_FieldAccess, var rhs : L4_FieldAccess) extends L4_Expression {
  override def prettyprint(out : PpStream) = out << lhs << " * " << rhs
  override def progress : IR_Expression = ???
}

/// L4_WrapFieldFieldConvolutions

object L4_WrapFieldFieldConvolutions extends DefaultStrategy("Wrap field-field-convolutions with reduction loops") {

  this += new Transformation("Handle field-field-convolutions", {
    case func : L4_Function =>
      func.body = processStmtList(func.body)
      func
  })

  def processStmtList(statements : ListBuffer[L4_Statement]) : ListBuffer[L4_Statement] = {
    import L4_MapReductionVariable._
    var newBody = ListBuffer[L4_Statement]()

    // reset counter for temporary variables
    // tmpVarCounter = 0

    // process each statement of the function's body
    for (stmt <- statements) {
      stmt match {
        case loop : L4_UntilLoop   => loop.body = processStmtList(loop.body)
        case loop : L4_WhileLoop   => loop.body = processStmtList(loop.body)
        case loop : L4_ForLoop     => loop.body = processStmtList(loop.body)
        case cond : L4_IfCondition =>
          cond.trueBody = processStmtList(cond.trueBody)
          cond.falseBody = processStmtList(cond.falseBody)

        // TODO: emit warning for types with list of statements as member

        case _ =>
          // check if statement includes a field-field-convolution; replace with temporary variable
          L4_MapReductionVariable.applyStandalone(stmt)

          if (tmpVarMap.nonEmpty) {
            // add temporary variable
            for (tmpVar <- tmpVarMap.toList.sortBy(_._1.name)) {
              // FIXME: data type - tmpVar._2.lhs.resolveField.gridDatatype // FIXME: default value according to data type
              //FIXME reduction variable always real
              newBody += L4_VariableDeclaration(tmpVar._1.name, None, tmpVar._1.datatype, Some(L4_RealConstant(0)), false)
            }

            // add reduction loop
            // TODO: merge loops/ communications for identical fields
            // TODO: warp in fragment loops?
            for (tmpVar <- tmpVarMap.toList.sortBy(_._1.name)) {
              val assignment = L4_Assignment(Duplicate(tmpVar._1), Duplicate(tmpVar._2), "+=", None)
              val red = L4_Reduction("+", tmpVar._1.name)
              val commStmts = ListBuffer[L4_Communicate]() // can be extended if comm is required - dup maybe?

              val loop = L4_LoopOverField(tmpVar._2.lhs, assignment)
              loop.reduction = Some(red)
              loop.preComms = commStmts

              newBody += loop
            }

            // consume map entries
            tmpVarMap.clear
          }
      }
      // add original stmt in any case
      newBody += stmt
    }

    newBody
  }

  object L4_MapReductionVariable extends QuietDefaultStrategy("Map reduction variables for field-field-convolutions") with ObjectWithState {
    var tmpVarCounter : Int = 0
    var tmpVarMap = HashMap[L4_VariableAccess, L4_FieldFieldConvolution]()

    override def clear() = {
      tmpVarCounter = 0
      tmpVarMap = HashMap()
    }

    this += new Transformation("resolve", {
      case conv : L4_FieldFieldConvolution =>
        // TODO: incorporate rhs's dt as well
        val datatype = conv.lhs.target.datatype
        val tmpVar = L4_PlainVariableAccess(s"reductionVar_$tmpVarCounter", datatype, false)
        tmpVarCounter += 1 // TODO: reset counter when leaving scope

        tmpVarMap += (tmpVar -> conv)

        Duplicate(tmpVar)
    })
  }

}

/// L4_UnresolveFieldFieldConvolutions

object L4_UnresolveFieldFieldConvolutions extends DefaultStrategy("Revert field field convolutions to plain multiplications") {
  this += new Transformation("Replace", {
    case L4_FieldFieldConvolution(lhs, rhs) => L4_Multiplication(lhs, rhs)
  })
}
