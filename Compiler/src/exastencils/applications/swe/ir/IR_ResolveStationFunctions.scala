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

package exastencils.applications.swe.ir

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.datastructures._
import exastencils.domain.ir.IR_ReadLineFromFile
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.logger.Logger

/// IR_ResolveStationFunctions

object IR_ResolveStationFunctions extends DefaultStrategy("ResolveStationFunctions") {
  var resolveId : Int = 0

  override def apply(applyAtNode : Option[Node]) : Unit = {
    resolveId = 0
    super.apply(applyAtNode)
  }

  override def applyStandalone(node : Node) = {
    resolveId = 0
    super.applyStandalone(node)
  }

  this += new Transformation("ResolveFunctionCalls", {
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("readStations", _), args)) =>
      if (args.size != 1
        || !(args.head.isInstanceOf[IR_StringConstant]
        || (args.head.isInstanceOf[IR_VariableAccess] && args.head.asInstanceOf[IR_VariableAccess].datatype == IR_StringDatatype))) {
        Logger.error("Malformed call to readStations; usage: readStations ( \"filename\" )")
      }

      if (!IR_GlobalCollection.get.functions.exists(_.name == "readStations")) {
        IR_ReadLineFromFile.addToUtil
        IR_UserFunctions.get.internalDependencies += IR_GlobalCollection.defHeader
        IR_UserFunctions.get.internalDependencies = IR_UserFunctions.get.internalDependencies.distinct
        IR_GlobalCollection.get.functions += IR_ReadStations()
        IR_GlobalCollection.get.externalDependencies += "iostream"
        IR_GlobalCollection.get.externalDependencies = IR_GlobalCollection.get.externalDependencies.distinct
      }

      IR_ExpressionStatement(IR_FunctionCall(IR_PlainInternalFunctionReference("readStations", IR_UnitDatatype), args))

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference(IR_WriteStations.basename, _), args)) =>
      val writeStation = IR_WriteStations(resolveId, args)
      IR_UserFunctions.get.internalDependencies += IR_GlobalCollection.defHeader
      IR_UserFunctions.get.internalDependencies = IR_UserFunctions.get.internalDependencies.distinct
      IR_GlobalCollection.get.functions += writeStation
      IR_GlobalCollection.get.externalDependencies += "iostream"
      IR_GlobalCollection.get.externalDependencies += "cmath"
      IR_GlobalCollection.get.externalDependencies = IR_GlobalCollection.get.externalDependencies.distinct

      resolveId += 1
      IR_ExpressionStatement(IR_FunctionCall(IR_PlainInternalFunctionReference(writeStation.name, IR_UnitDatatype), args.head))
  })
}
