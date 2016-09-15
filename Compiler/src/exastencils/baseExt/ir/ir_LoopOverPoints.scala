package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.communication.CommunicateStatement
import exastencils.core.collectors.StackCollector
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.{ Reduction, _ }
import exastencils.knowledge._
import exastencils.logger.Logger
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.prettyprinting.PpStream

/// IR_RegionSpecification

case class IR_RegionSpecification(var region : String, var dir : IR_ConstIndex, var onlyOnBoundary : Boolean)

/// IR_LoopOverPoints

case class IR_LoopOverPoints(var field : Field,
    var region : Option[IR_RegionSpecification],
    var seq : Boolean, // FIXME: seq HACK
    var startOffset : IR_ExpressionIndex,
    var endOffset : IR_ExpressionIndex,
    var increment : IR_ExpressionIndex,
    var body : ListBuffer[IR_Statement],
    var preComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var postComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var reduction : Option[Reduction] = None,
    var condition : Option[IR_Expression] = None) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPoints\n"

  def expandSpecial(collector : StackCollector) : Output[StatementList] = {
    val insideFragLoop = collector.stack.map({ case loop : IR_LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
    val innerLoop =
      if (Knowledge.experimental_splitLoopsForAsyncComm)
        IR_LoopOverPointsInOneFragment(field.domain.index, field, region, seq, startOffset, endOffset, increment, body, preComms, postComms, reduction, condition)
      else
        IR_LoopOverPointsInOneFragment(field.domain.index, field, region, seq, startOffset, endOffset, increment, body, ListBuffer(), ListBuffer(), reduction, condition)

    var stmts = ListBuffer[IR_Statement]()
    stmts += innerLoop

    if (!insideFragLoop)
      if (seq)
        stmts = ListBuffer(new IR_LoopOverFragments(stmts, reduction))
      else
        stmts = ListBuffer(new IR_LoopOverFragments(stmts, reduction) with OMP_PotentiallyParallel)

    if (Knowledge.experimental_splitLoopsForAsyncComm)
      stmts
    else {
      if (preComms.nonEmpty) Logger.warn("Found precomm")
      preComms ++ stmts ++ postComms
    }
  }
}
