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

import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.parallelization.ir.IR_PotentiallyCritical
import exastencils.prettyprinting.PpStream

/// OMP_Critical

object OMP_Critical {
  var counter = 0
}

case class OMP_Critical(var body : IR_PotentiallyCritical) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    import OMP_Critical.counter

    out << "#pragma omp critical"
    if (Knowledge.omp_nameCriticalSections) {
      out << " (section_" << counter << ")"
      counter += 1
    }

    out << '\n' << body
  }
}

/// OMP_AddCriticalSections

object OMP_AddCriticalSections extends DefaultStrategy("Resolve potentially critical omp sections") {
  this += new Transformation("Adding OMP critical pragmas", {
    case target : IR_PotentiallyCritical => OMP_Critical(target)
  }, false) // turn off recursion due to wrapping mechanism
}
