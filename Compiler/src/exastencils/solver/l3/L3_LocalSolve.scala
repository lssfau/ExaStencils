package exastencils.solver.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.prettyprinting._
import exastencils.solver.l4._

/// L3_LocalSolve

object L3_LocalSolve {
  // parser interface
  def apply(unknowns : List[L3_UnresolvedAccess], equations : List[L3_Equation], jacobiType : Boolean, relax : Option[L3_Expression], fieldForLoop : L3_Access)
  = new L3_LocalSolve(unknowns.map(_.asInstanceOf[L3_Expression]).to[ListBuffer], equations.to[ListBuffer], jacobiType, relax, fieldForLoop)
}

case class L3_LocalSolve(
    var unknowns : ListBuffer[L3_Expression],
    var equations : ListBuffer[L3_Equation],
    var jacobiType : Boolean,
    var relax : Option[L3_Expression],
    var fieldForLoop : L3_Access) extends L3_Statement {

  override def prettyprint(out : PpStream) : Unit = {
    out << "solve locally "
    if (jacobiType) out << "with jacobi "
    if (relax.isDefined) out << "relax " << relax.get << ' '
    out << "{\n"
    for (i <- unknowns.indices)
      out << unknowns(i) << " => " << equations(i) << "\n"
    out << "}"
  }

  override def progress = {
    val ret = L4_LocalSolve(
      unknowns.map(_.progress),
      equations.map(_.progress),
      jacobiType,
      L3_ProgressOption(relax)(_.progress))

    ret.fieldForLoop = fieldForLoop.progress

    ret
  }
}