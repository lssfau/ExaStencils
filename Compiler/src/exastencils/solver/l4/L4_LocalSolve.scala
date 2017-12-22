package exastencils.solver.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.prettyprinting._
import exastencils.solver.ir._

/// L4_LocalSolve

object L4_LocalSolve {
  // parser interface
  def apply(unknowns : List[L4_UnresolvedAccess], equations : List[L4_Equation], jacobiType : Boolean, relax : Option[L4_Expression])
  = new L4_LocalSolve(unknowns.map(_.asInstanceOf[L4_Expression]).to[ListBuffer], equations.to[ListBuffer], jacobiType, relax)
}

case class L4_LocalSolve(
    var unknowns : ListBuffer[L4_Expression],
    var equations : ListBuffer[L4_Equation],
    var jacobiType : Boolean,
    var relax : Option[L4_Expression]) extends L4_Statement {

  // used to generate loop over field when coming from L3
  var fieldForLoop : L4_Access = _

  override def prettyprint(out : PpStream) : Unit = {
    out << "solve locally "
    if (jacobiType) out << "with jacobi "
    if (relax.isDefined) out << "relax " << relax.get << ' '
    out << "{\n"
    for (i <- unknowns.indices)
      out << unknowns(i) << " => " << equations(i) << "\n"
    out << "}"
  }

  override def progress = ProgressLocation {
    IR_LocalSolve(
      unknowns.map(_.progress.asInstanceOf[IR_FieldAccess]),
      equations.map(_.progress),
      jacobiType,
      L4_ProgressOption(relax)(_.progress))
  }
}

/// L4_AddLoopsToLocalSolve

object L4_AddLoopsToLocalSolve extends DefaultStrategy("Add loop statements around local solve statements") {
  this += new Transformation("Add loops", {
    case solve : L4_LocalSolve =>
      L4_LoopOverField(Duplicate(solve.fieldForLoop), solve)
  }, false /* recursion must be switched of due to wrapping mechanism */)
}
