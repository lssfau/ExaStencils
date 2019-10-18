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

package exastencils.core.collectors

import exastencils.datastructures._

/**
  * Keeps track of [[exastencils.datastructures.Node]]s that are traversed during the application of [[exastencils.datastructures.Transformation]]s.
  *
  * Collectors follow the Observer-pattern. They are added to the [[exastencils.core.StateManager]], who will notify
  * the observers whenever a [[exastencils.datastructures.Node]] is entered or left during the traversal of the program
  * state. This is a basic trait that must be implemented by concrete implementations.
  */
trait Collector {

  /**
    * Specifies if this collector can be applied concurrently to different functions of the AST. Default is false.
    */
  var isParallel : Boolean = false

  /**
    * Is called when a new [[exastencils.datastructures.Node]] is visited.
    *
    * @param node The [[exastencils.datastructures.Node]] currently visited.
    */
  def enter(node : Node) : Unit

  /**
    * Is called when traversal of a [[exastencils.datastructures.Node]] is done and it is left.
    *
    * @param node The [[exastencils.datastructures.Node]] just left.
    */
  def leave(node : Node) : Unit

  /**
    * Resets the internal state of the Collector.
    *
    * This method resets the internal state of the Collector.
    * [[exastencils.core.StateManager]] calls this method before applying a new [[exastencils.datastructures.Transformation]].
    * This may be used to clear the history of a Collector that keeps a protocol of the visited [[exastencils.datastructures.Node]]s.
    */
  def reset() : Unit
}
