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

package exastencils.parallelization.api.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.datastructures.Node
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// OMP_Clause

abstract class OMP_Clause extends Node with PrettyPrintable

/// OMP_Schedule

case class OMP_Schedule() extends OMP_Clause {

  // mainly error checking. from spec @ v5.1
  def schedule = {
    val modifiers = List("monotonic", "nonmonotonic", "simd")
    val kinds = List("static", "dynamic", "guided", "auto", "runtime")

    // signature: schedule([modifier [, modifier]:]kind[, chunk_size])
    val pattern = """(?:[A-Za-z]+\s*(?:,\s*[A-Za-z]+)?:)?\s*([A-Za-z]+)\s*(?:,\s*([0-9]+))?""".r
    Knowledge.omp_scheduling match {
      case pattern(kind) if kinds.contains(kind) =>
      case pattern(kind, _) if kinds.contains(kind) =>
      case pattern(mod, kind) if modifiers.contains(mod) && kinds.contains(kind) =>
      case pattern(mod, kind, _) if modifiers.contains(mod) && kinds.contains(kind) =>
      case pattern(mod, mod2, kind) if List(mod, mod2).forall(modifiers.contains) && kinds.contains(kind) =>
      case pattern(mod, mod2, kind, _) if List(mod, mod2).forall(modifiers.contains) && kinds.contains(kind) =>
      case _ => Logger.error("OMP_ParallelFor: Invalid Knowledge parameter: omp_scheduling = " + Knowledge.omp_scheduling)
    }
    Knowledge.omp_scheduling
  }

  override def prettyprint(out : PpStream) : Unit = out << schedule
}

/// OMP_Reduction

object OMP_Reduction {
  def apply(red : IR_Reduction) = new OMP_Reduction(red.op, red.target, red.targetName)
}

case class OMP_Reduction(var op : String, var target : IR_Expression, var targetName : String) extends OMP_Clause {
  override def prettyprint(out : PpStream) : Unit = {
    val acc = target.datatype match {
      case dt : IR_MatrixDatatype =>
        target match {
          case acc : IR_VariableAccess =>
            // matrix variable access: OpenMP >= 4.5 offers a more compact notation
            if (Platform.omp_version >= 4.5)
              IR_Native(s"${acc.name}[:${dt.sizeM * dt.sizeN}]")
            else
              Logger.error("Matrix reduction for OpenMP < 4.5 are currently unsupported.")
          case acc : IR_ArrayAccess    =>
            // (resolved) matrix element access: use base datatype
            acc
          case acc                     =>
            acc
        }
      case _ =>
        target
    }

    out << "reduction(" << op << " : " << acc << ')'
  }
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
