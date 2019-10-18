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

import exastencils.logger._

/**
  * A Strategy that executes its [[exastencils.datastructures.Transformation]]s sequentially.
  * It suppresses all debug output, when called in standalone mode.
  *
  * @param name Name of the Strategy. Used for traceability and debugging purposes.
  */
class QuietDefaultStrategy(name : String) extends DefaultStrategy(name) {

  override def applyStandalone(node : Node) : Unit = {
    Logger.pushLevel(Logger.WARNING)
    try {
      super.applyStandalone(node)
    } finally {
      Logger.popLevel()
    }
  }
}

object QuietDefaultStrategy {

  /**
    * A Strategy that executes its [[exastencils.datastructures.Transformation]]s sequentially.
    * It suppresses all debug output, when called in standalone mode.
    *
    * @param name Name of the Strategy. Used for traceability and debugging purposes.
    */
  def apply(name : String) = new QuietDefaultStrategy(name)

  /**
    * A Strategy that executes its [[exastencils.datastructures.Transformation]]s sequentially.
    * It suppresses all debug output, when called in standalone mode.
    *
    * @param name            Name of the Strategy. Used for traceability and debugging purposes.
    * @param transformations List of transformations for the strategy.
    */
  def apply(name : String, transformations : Transformation*) = {
    val s = new QuietDefaultStrategy(name)
    s ++= transformations
    s
  }
}
