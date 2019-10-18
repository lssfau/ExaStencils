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

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("writeStations", _), args)) =>
      // TODO: optimize
      val writeStationFctName = (0 until 1000).toArray.map(i => "writeStations" + i).find(name => !IR_GlobalCollection.get.functions.exists(_.name == name)) match {
        case Some(v) => v
        case None    => Logger.error("Too many writeStation calls. Cannot build more writeStation-functions.")
      }

      IR_UserFunctions.get.internalDependencies += IR_GlobalCollection.defHeader
      IR_UserFunctions.get.internalDependencies = IR_UserFunctions.get.internalDependencies.distinct
      IR_GlobalCollection.get.functions += IR_WriteStations(writeStationFctName, args)
      IR_GlobalCollection.get.externalDependencies += "iostream"
      IR_GlobalCollection.get.externalDependencies += "cmath"
      IR_GlobalCollection.get.externalDependencies = IR_GlobalCollection.get.externalDependencies.distinct

      IR_ExpressionStatement(IR_FunctionCall(IR_PlainInternalFunctionReference(writeStationFctName, IR_UnitDatatype), args.head))
  })
}
