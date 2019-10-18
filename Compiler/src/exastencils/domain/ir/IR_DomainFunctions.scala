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

package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.communication.ir.IR_CommunicationFunctions
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.grid.ir._
import exastencils.logger.Logger

/// IR_DomainFunctions

object IR_DomainFunctions extends ObjectWithState {
  def defBaseName = "Domain/Domain"
  def defHeader = defBaseName + ".h"

  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_CommunicationFunctions] = None

  override def clear() = { selfRef = None }

  // looks itself up starting from the current root
  def get = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_CommunicationFunctions]()
    selfRef.get
  }
}

case class IR_DomainFunctions() extends IR_FunctionCollection(IR_DomainFunctions.defBaseName,
  ListBuffer(),
  ListBuffer(IR_GlobalCollection.defHeader, IR_CommunicationFunctions.defHeader)) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"

  if (Knowledge.domain_rect_generate)
    functions += IR_InitGeneratedDomain()
  else if (Knowledge.domain_readFromFile)
    functions += IR_InitDomainFromFile()
  else
    Logger.error("Unsupported domain configuration")

  // assemble init statements for applicable virtual fields
  { // scope to avoid initStmts being a member which is later targeted by transformations
    val initStmts = ListBuffer[IR_Statement]()

    var dependencies = IR_VirtualFieldCollection.objects.map(vf => (vf, vf.generateInitCodeDependsOn()))
    while (dependencies.nonEmpty) {
      val (canBeDone, waiting) = dependencies.partition(_._2.isEmpty)
      initStmts ++= canBeDone.flatMap(_._1.generateInitCode())
      dependencies = waiting.map(e => (e._1, e._2.filterNot(canBeDone.map(_._1).contains)))
    }

    functions += IR_PlainFunction("initGeometry", IR_UnitDatatype, initStmts)
  }
}
