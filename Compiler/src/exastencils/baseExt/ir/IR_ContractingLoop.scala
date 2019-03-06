package exastencils.baseExt.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_FieldSelection
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
        case fs @ IR_FieldSelection(field, level, IR_SlotAccess(slot, offset), _) =>
          fs.slot = IR_SlotAccess(slot, offset + fieldOffset.getOrElse(FieldKey(field), 0))
          fs
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
    // iterate in reverse order to allow a proper counting
    //  (which is required for colored smoothers: bounds must be adapted per LoopOverDims, not per iteration of the ContractingLoop)
    var expand : Int = 0
    for (iteration <- (number - 1) to 0 by -1) {
      replIt.itVal = iteration
      for (stmt <- body.reverseIterator)
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
                  res.prepend(condStmt)
                }
                replIt.applyStandalone(nju)
                condStmt.trueBody.prepend(nju)
                expand += 1
              case _                                     =>
                Logger.error("IR_ContractingLoop cannot be expanded: body contains an IR_IfCondition with unexpected statements")
            }

          case l : IR_LoopOverDimensions =>
            val nju = processLoopOverDimensions(l, expand, fieldOffset)
            replIt.applyStandalone(nju)
            res.prepend(nju)
            expand += 1
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
