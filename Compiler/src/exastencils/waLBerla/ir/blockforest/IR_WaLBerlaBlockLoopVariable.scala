package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.grid.IR_WaLBerlaCellWidthBlockPerDim

// implicit ordering for block-loop variables
object IR_WaLBerlaBlockLoopVariable {
  // order by type
  val byTypeOrd : Ordering[IR_WaLBerlaBlockLoopVariable] = Ordering.by {
    case _ : IR_WaLBerlaRefinementLevel      => 0
    case _ : IR_WaLBerlaCellWidthBlockPerDim => 1
    case _                                   => 2
  }
  // order by name
  val byNameOrd : Ordering[IR_WaLBerlaBlockLoopVariable] = Ordering.by { member : IR_WaLBerlaBlockLoopVariable => member.resolveName() }

  // combine both orderings
  implicit val ord = Ordering.by { member : IR_WaLBerlaBlockLoopVariable => (member, member) }(Ordering.Tuple2(byTypeOrd, byNameOrd))
}

trait IR_WaLBerlaBlockLoopVariable extends IR_Expression {
  def resolveName() : String
  def resolveDatatype() : IR_Datatype

  def resolveAccess() : IR_VariableAccess = IR_VariableAccess(resolveName(), resolveDatatype())

  def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName())

  override def datatype = resolveDatatype()
  override def prettyprint(out : PpStream) : Unit = out << resolveName
}

object IR_WaLBerlaFindBlockLoopVariables extends QuietDefaultStrategy("Find accesses with refinement") {
  var blockLoopVariables : ListBuffer[IR_WaLBerlaBlockLoopVariable] = ListBuffer()

  override def applyStandalone(node : Node) : Unit = {
    blockLoopVariables.clear()
    super.applyStandalone(node)
  }

  this += Transformation("..", {
    case acc : IR_WaLBerlaBlockLoopVariable =>
      blockLoopVariables += acc
      acc
  })
}
