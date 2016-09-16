package exastencils.baseExt.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.data._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream
import exastencils.util.SimplifyExpression

/// IR_ContractionSpecification

case class IR_ContractionSpecification(var posExt : IR_ConstIndex, var negExt : IR_ConstIndex)

/// IR_ContractingLoop

case class IR_ContractingLoop(var number : Int, var iterator : Option[IR_Expression], var statements : ListBuffer[IR_Statement],
    var spec : IR_ContractionSpecification) extends IR_Statement {
  // FIXME: iterator is not used?!
  // TODO: validate spec
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsBegin(expr : IR_Expression, extent : Int) : IR_Expression = {
    expr match {
      case e if Knowledge.experimental_useStefanOffsets =>
        e // don't do anything here

      case IR_IntegerConstant(i) =>
        IR_IntegerConstant(i - extent)

      case bOff @ IR_BoundedScalar(_, _, IR_ArrayAccess(_ : iv.IterationOffsetBegin, _, _)) =>
        (bOff * (extent + 1)) - extent

      case add : IR_AdditionExpression =>
        add.summands.transform {
          case bOff @ IR_BoundedScalar(_, _, IR_ArrayAccess(_ : iv.IterationOffsetBegin, _, _)) =>
            bOff * (extent + 1)
          case x                                                                                =>
            x
        }
        add.summands += IR_IntegerConstant(-extent)
        SimplifyExpression.simplifyIntegralExpr(add)

      // case oInd @ OffsetIndex(0, 1, _, ArrayAccess(_ : iv.IterationOffsetBegin, _, _)) =>
      //   oInd.maxOffset += extent
      //   oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index - extent)
      //   oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
      //   oInd
    }
  }

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsEnd(expr : IR_Expression, extent : Int) : IR_Expression = {
    expr match {
      case e if Knowledge.experimental_useStefanOffsets =>
        e // don't do anything here

      case IR_IntegerConstant(i) =>
        IR_IntegerConstant(i + extent)

      case bOff @ IR_BoundedScalar(_, _, IR_ArrayAccess(_ : iv.IterationOffsetEnd, _, _)) =>
        (bOff * (extent + 1)) + extent

      case add : IR_AdditionExpression =>
        add.summands.transform {
          case bOff @ IR_BoundedScalar(_, _, IR_ArrayAccess(_ : iv.IterationOffsetEnd, _, _)) =>
            bOff * (extent + 1)
          case x                                                                              =>
            x
        }
        add.summands += IR_IntegerConstant(extent)
        SimplifyExpression.simplifyIntegralExpr(add)

      // case oInd @ OffsetIndex(-1, 0, _, ArrayAccess(_ : iv.IterationOffsetEnd, _, _)) =>
      //   oInd.minOffset -= extent
      //   oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index + extent)
      //   oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
      //   oInd
    }
  }

  private type FieldKey = (String, Int)
  private def FieldKey(field : Field) : FieldKey = (field.identifier, field.level)

  private def updateSlots(stmts : ListBuffer[IR_Statement], fieldOffset : HashMap[FieldKey, Int]) : Unit = {
    object AdaptFieldSlots extends QuietDefaultStrategy("Adapt field slots") {
      this += new Transformation("now", {
        case fs @ FieldSelection(field, level, SlotAccess(slot, offset), _, _) =>
          fs.slot = SlotAccess(slot, offset + fieldOffset.getOrElse(FieldKey(field), 0))
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
    val res = new ListBuffer[IR_Statement]()
    val fieldOffset = new HashMap[FieldKey, Int]()
    val fields = new HashMap[FieldKey, Field]()
    var condStmt : IR_IfCondition = null
    for (i <- 1 to number)
      for (stmt <- statements)
        stmt match {
          case AdvanceSlotStatement(iv.CurrentSlot(field, fragment)) =>
            val fKey = FieldKey(field)
            fieldOffset(fKey) = fieldOffset.getOrElse(fKey, 0) + 1
            fields(fKey) = field

          case cStmt @ IR_IfCondition(cond, trueBody : ListBuffer[IR_Statement], ListBuffer()) =>
            val bodyWithoutComments = trueBody.filterNot(x => x.isInstanceOf[IR_Comment])
            bodyWithoutComments match {
              case ListBuffer(l : IR_LoopOverDimensions) =>
                val nju = processLoopOverDimensions(l, number - i, fieldOffset)
                if (condStmt == null || cond != condStmt.condition) {
                  condStmt = Duplicate(cStmt)
                  condStmt.trueBody.clear()
                  res += condStmt
                }
                condStmt.trueBody += nju
              case _                                     =>
            }

          case l : IR_LoopOverDimensions =>
            res += processLoopOverDimensions(l, number - i, fieldOffset)
        }

    for ((fKey, offset) <- fieldOffset) {
      val field = fields(fKey)
      res += IR_Assignment(iv.CurrentSlot(field), (iv.CurrentSlot(field) + offset) Mod field.numSlots)
    }

    res
  }
}
