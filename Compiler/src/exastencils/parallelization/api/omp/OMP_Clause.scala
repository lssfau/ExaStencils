package exastencils.parallelization.api.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Node
import exastencils.prettyprinting._

/// OMP_Clause

abstract class OMP_Clause extends Node with PrettyPrintable

/// OMP_Reduction

object OMP_Reduction {
  def apply(red : IR_Reduction) = new OMP_Reduction(red.op, red.target)
}

case class OMP_Reduction(var op : String, var target : IR_VariableAccess) extends OMP_Clause {
  override def prettyprint(out : PpStream) : Unit = out << "reduction(" << op << " : " << target << ')'
}

/// OMP_LastPrivate

case class OMP_LastPrivate(var vars : ListBuffer[IR_VariableAccess]) extends OMP_Clause {
  def this(v : IR_VariableAccess) = this(ListBuffer(v))
  override def prettyprint(out : PpStream) : Unit = out << "lastprivate(" <<< (vars, ", ") << ')'
}

/// OMP_Private

case class OMP_Private(var vars : ListBuffer[IR_VariableAccess]) extends OMP_Clause {
  def this(v : IR_VariableAccess) = this(ListBuffer(v))
  override def prettyprint(out : PpStream) : Unit = out << "private(" <<< (vars, ", ") << ')'
}
