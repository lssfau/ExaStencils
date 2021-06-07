package exastencils.waLBerla.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Statement
import exastencils.base.l4.L4_Expression
import exastencils.base.l4.L4_ExpressionIndex
import exastencils.base.l4.L4_ProgressOption
import exastencils.base.l4.L4_Reduction
import exastencils.base.l4.L4_Statement
import exastencils.baseExt.ir.IR_LoopOverPoints
import exastencils.baseExt.l4.L4_LoopOverField
import exastencils.baseExt.l4.L4_RegionSpecification
import exastencils.communication.l4.L4_Communicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4.L4_FieldAccess
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaLoopOverPoints


case class L4_WaLBerlaLoopOverField(
    var fieldAcc : L4_FieldAccess,
    var region : Option[L4_RegionSpecification],
    var seq : Boolean, // FIXME: seq HACK
    var condition : Option[L4_Expression],
    var startOffset : Option[L4_ExpressionIndex],
    var endOffset : Option[L4_ExpressionIndex],
    var increment : Option[L4_ExpressionIndex],
    var body : ListBuffer[L4_Statement],
    var reduction : Option[L4_Reduction],
    var preComms : ListBuffer[L4_Communicate],
    var postComms : ListBuffer[L4_Communicate]) extends L4_Statement {

  // reuse most of L4_LoopOverField impl
  lazy val loopOverField : IR_LoopOverPoints =
    L4_LoopOverField(L4_FieldAccess(fieldAcc.target, fieldAcc.slot, fieldAcc.offset, fieldAcc.arrayIndex, fieldAcc.frozen, fieldAcc.matIndex)).progress

  override def prettyprint(out : PpStream) : Unit = {
    out << loopOverField.prettyprint()
  }

  lazy val wbField = L4_WaLBerlaFieldCollection.getByFieldAccess(fieldAcc).get

  override def progress : IR_Statement = {
    val newloop = IR_WaLBerlaLoopOverPoints(
      wbField.progress(),
      if (region.isDefined) Some(region.get.progress) else None,
      loopOverField.startOffset,
      loopOverField.endOffset,
      loopOverField.increment,
      body.map(_.progress),
      preComms.map(_.progress),
      postComms.map(_.progress),
      loopOverField.parallelization,
      L4_ProgressOption(condition)(_.progress))

    newloop.annotate("l4_fromDSL") // experimental annotation -> if successful and performance impacts are ok annotate all l4 statements
    newloop
  }
}

/// L4_WaLBerlaReplaceLoopOverField

object L4_WaLBerlaResolveLoopOverField extends DefaultStrategy("Resolve LoopOverField for WB fields") {
  this += Transformation("Resolve", {
    case L4_LoopOverField(fAcc : L4_FieldAccess, region, seq, condition, start, end, incr, body, reduction, preComms, postComms) if L4_WaLBerlaFieldCollection.contains(fAcc) =>
      // resolve accesses to waLBerla fields
      val wbField = L4_WaLBerlaFieldCollection.getByFieldAccess(fAcc).get // get field from wb field collection
      val newAcc = L4_FieldAccess(wbField.toField, fAcc.slot, fAcc.offset, fAcc.arrayIndex, fAcc.frozen, fAcc.matIndex) // create 'regular' access for it

      L4_WaLBerlaLoopOverField(newAcc, region, seq, condition, start, end, incr, body, reduction, preComms, postComms)
  })
}