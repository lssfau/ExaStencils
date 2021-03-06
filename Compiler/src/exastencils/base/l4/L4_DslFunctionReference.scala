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

package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l4.L4_LevelCollector

/// L4_PlainDslFunctionReference

case class L4_PlainDslFunctionReference(var name : String, var returnType : L4_Datatype) extends L4_PlainFunctionReference {
  override def progress = ProgressLocation(IR_PlainDslFunctionReference(name, returnType.progress))
}

/// L4_LeveledDslFunctionReference

case class L4_LeveledDslFunctionReference(var name : String, var level : Int, var returnType : L4_Datatype) extends L4_LeveledFunctionReference {
  override def progress = ProgressLocation(IR_LeveledDslFunctionReference(name, level, returnType.progress))
}

/// L4_ResolveDslFunctionReferences

object L4_ResolveDslFunctionReferences extends DefaultStrategy("Resolve function references") {
  val declCollector = L4_FunctionCollector
  this.register(declCollector)

  val levelCollector = new L4_LevelCollector
  this.register(levelCollector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Collecting function declarations", PartialFunction.empty)

  this += new Transformation("Resolve", {
    case ref : L4_UnresolvedFunctionReference if declCollector.exists(ref.name) =>
      // check for levels in ref and decl
      if (declCollector.existsPlain(ref.name)) {
        // access to plain function
        if (ref.level.nonEmpty) Logger.warn(s"Level access in function call to un-leveled function ${ ref.name } will be ignored")
        L4_PlainDslFunctionReference(ref.name, declCollector.getDatatype(ref.name))
      } else {
        // access to leveled function
        val lvl = {
          if (ref.level.isDefined) ref.level.get.resolveLevel
          else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
          else Logger.error(s"Missing level for calling of ${ ref.name }")
        }

        L4_LeveledDslFunctionReference(ref.name, lvl, declCollector.getDatatype(ref.name, lvl))
      }
  })
}
