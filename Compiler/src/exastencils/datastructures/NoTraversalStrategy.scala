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

package exastencils.datastructures

import exastencils.core.TransformationException
import exastencils.logger.Logger

abstract class NoTraversalStrategy(name : String) extends Strategy(name) {
  def apply() = {
    this.onBefore()
    this.transaction()
    Logger.info(s"""Applying strategy "${ name }"""")
    try {
      doWork()
      this.commit()
    } catch {
      case x : TransformationException =>
        Logger.warn(s"""Strategy "${ name }" did not apply successfully""")
        Logger.warn(s"""Error in Transformation ${ x.transformation.name }""")
        Logger.warn(s"Message: ${ x.msg }")
        Logger.warn(s"Rollback will be performed")
        this.abort()
    }
    this.onAfter()
  }

  def apply(applyAtNode : Option[Node]) : Unit = apply()

  def doWork()
}
