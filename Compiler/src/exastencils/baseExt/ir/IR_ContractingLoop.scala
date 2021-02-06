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

package exastencils.baseExt.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

/// IR_ContractionSpecification

case class IR_ContractionSpecification(var posExt : IR_ConstIndex, var negExt : IR_ConstIndex) extends IR_Node

/// IR_ContractingLoop

case class IR_ContractingLoop(var number : Int, var iterator : Option[IR_Expression], var body : ListBuffer[IR_Statement],
    var spec : IR_ContractionSpecification) extends IR_Statement with IR_SpecialExpandable {
  // TODO: validate spec

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsBegin(expr : IR_Expression, extent : Int) : IR_Expression = {
    expr match {
      case e if Knowledge.experimental_useStefanOffsets =>
        e // don't do anything here

      case IR_IntegerConstant(i) =>
        IR_IntegerConstant(i - extent)

      case bOff @ IR_BoundedScalar(_, _, _ : IR_IV_IterationOffsetBegin) =>
        (bOff * (extent + 1)) - extent

      case add : IR_Addition =>
        add.summands.transform {
          case bOff @ IR_BoundedScalar(_, _, _ : IR_IV_IterationOffsetBegin) =>
            bOff * (extent + 1)
          case x                                                             =>
            x
        }
        add.summands += IR_IntegerConstant(-extent)
        IR_SimplifyExpression.simplifyIntegralExpr(add)
    }
  }

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsEnd(expr : IR_Expression, extent : Int) : IR_Expression = {
    expr match {
      case e if Knowledge.experimental_useStefanOffsets =>
        e // don't do anything here

      case IR_IntegerConstant(i) =>
        IR_IntegerConstant(i + extent)

      case bOff @ IR_BoundedScalar(_, _, _ : IR_IV_IterationOffsetEnd) =>
        (bOff * (extent + 1)) + extent

      case add : IR_Addition =>
        add.summands.transform {
          case bOff @ IR_BoundedScalar(_, _, _ : IR_IV_IterationOffsetEnd) =>
            bOff * (extent + 1)
          case x                                                           =>
            x
        }
        add.summands += IR_IntegerConstant(extent)
        IR_SimplifyExpression.simplifyIntegralExpr(add)
    }
  }

  private type FieldKey = (String, Int)
  private def FieldKey(field : IR_Field) : FieldKey = (field.name, field.level)

  private def updateSlots(stmts : ListBuffer[IR_Statement], fieldOffset : HashMap[FieldKey, Int]) : Unit = {
    object AdaptFieldSlots extends QuietDefaultStrategy("Adapt field slots") {
      this += new Transformation("now", {
        case fa @ IR_FieldAccess(field, IR_SlotAccess(slot, offset), _, _, _, _, _) =>
          fa.slot = IR_SlotAccess(slot, offset + fieldOffset.getOrElse(FieldKey(field), 0))
          fa

        case fa @ IR_DirectFieldAccess(field, IR_SlotAccess(slot, offset), _, _) =>
          fa.slot = IR_SlotAccess(slot, offset + fieldOffset.getOrElse(FieldKey(field), 0))
          fa
      })
    }
    AdaptFieldSlots.applyStandalone(stmts)
  }

  def processLoopOverDimensions(l : IR_LoopOverDimensions, extent : Int, fieldOffset : HashMap[FieldKey, Int]) : IR_LoopOverDimensions = {
    val nju : IR_LoopOverDimensions = Duplicate(l)
    for (dim <- 0 until nju.numDimensions) {
      nju.indices.begin(dim) = extendBoundsBegin(nju.indices.begin(dim), extent * spec.negExt(dim))
      nju.indices.end(dim) = extendBoundsEnd(nju.indices.end(dim), extent * spec.posExt(dim))
    }
    updateSlots(nju.body, fieldOffset)
    nju
  }

  def expandSpecial : Output[NodeList] = {
    // unroll this loop and update the loop boundaries (saving communication requires a local copmutation of some ghost layers)
    val res = new ListBuffer[IR_Statement]()
    val fieldOffset = new HashMap[FieldKey, Int]()
    val fields = new HashMap[FieldKey, IR_Field]()
    var condStmt : IR_IfCondition = null
    val replIt = new QuietDefaultStrategy("replace Iterator") {
      var itVal : Long = 0
      this += new Transformation("now", {
        case expr : IR_Expression if expr == iterator.get =>
          IR_IntegerConstant(itVal)
      })
      override def applyStandalone(node : Node) : Unit = if (iterator.isDefined) super.applyStandalone(node)
    }
    // determine the inital expand value (it must be decreased for every IR_LoopOverDimensions node and it must reach 0 eventually)
    var expand : Int = -1 + number * body.view.flatMap({
      case IR_IfCondition(_, trueBody : ListBuffer[IR_Statement], ListBuffer()) => trueBody
      case x                                                                    => List(x)
    }).count({
      case _ : IR_LoopOverDimensions => true
      case _                         => false
    })
    for (iteration <- 0 until number) {
      replIt.itVal = iteration
      for (stmt <- body)
        stmt match {
          case IR_AdvanceSlot(IR_IV_ActiveSlot(field, fragment), step) =>
            val fKey = FieldKey(field)
            fieldOffset(fKey) = fieldOffset.getOrElse(fKey, 0) + step
            fields(fKey) = field

          case cStmt @ IR_IfCondition(cond, trueBody : ListBuffer[IR_Statement], ListBuffer()) =>
            val bodyWithoutComments = trueBody.filterNot(x => x.isInstanceOf[IR_Comment])
            bodyWithoutComments match {
              case ListBuffer(l : IR_LoopOverDimensions) =>
                val nju = processLoopOverDimensions(l, expand, fieldOffset)
                if (condStmt == null || cond != condStmt.condition) {
                  condStmt = Duplicate(cStmt)
                  condStmt.trueBody.clear()
                  replIt.applyStandalone(condStmt)
                  res += condStmt
                }
                replIt.applyStandalone(nju)
                condStmt.trueBody += nju
                expand -= 1
              case _                                     =>
                Logger.error("IR_ContractingLoop cannot be expanded: body contains an IR_IfCondition with unexpected statements")
            }

          case l : IR_LoopOverDimensions =>
            val nju = processLoopOverDimensions(l, expand, fieldOffset)
            replIt.applyStandalone(nju)
            res += nju
            expand -= 1
        }
    }

    for ((fKey, offset) <- fieldOffset) {
      val field = fields(fKey)
      res += IR_AdvanceSlot(IR_IV_ActiveSlot(field), offset)
    }

    res
  }
}

/// IR_ResolveContractingLoop

// Note: Must run after IR_ResolveLoopOverPointsInOneFragment
object IR_ResolveContractingLoop extends DefaultStrategy("Resolve ContractingLoop nodes") {
  this += new Transformation("Resolve", {
    case loop : IR_ContractingLoop => loop.expandSpecial
  })
}
