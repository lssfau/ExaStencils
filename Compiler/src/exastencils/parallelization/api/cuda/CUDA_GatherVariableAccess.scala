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

package exastencils.parallelization.api.cuda

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.datastructures._

object CUDA_GatherVariableAccess extends QuietDefaultStrategy("Gather local VariableAccess nodes") {
  var accesses = mutable.HashMap[String, IR_VariableAccess]()
  var ignoredAccesses = mutable.SortedSet[String]()

  def clear() = {
    accesses = mutable.HashMap[String, IR_VariableAccess]()
    ignoredAccesses += "std::cout"
    ignoredAccesses += "std::cerr"
    ignoredAccesses += "std::endl"
  }

  this += new Transformation("Searching", {
    case decl : IR_VariableDeclaration =>
      ignoredAccesses += decl.name
      decl

    case access : IR_VariableAccess if !ignoredAccesses.contains(access.name) =>
      accesses.put(access.name, access)
      access
  })
}
