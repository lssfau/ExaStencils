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

package exastencils.core

import scala.collection.immutable.HashMap

import exastencils.datastructures.RootNode
import exastencils.logger.Logger

// ###############################################################################################
// #### Checkpointing ############################################################################
// ###############################################################################################

object CheckpointManager {
  /** Type that represents a checkpoint identifier. */
  type CheckpointIdentifier = String
  protected var checkpoints_ = new HashMap[CheckpointIdentifier, RootNode]

  /**
    * Creates a new checkpoint (snapshot copy) of the current program state.
    *
    * @param id The identifier to be used for the newly created checkpoint.
    */
  def checkpoint(id : CheckpointIdentifier) : Unit = {
    Logger.debug(s"""Creating checkpoint "$id"""")
    try {
      var c = Duplicate(StateManager.root)
      checkpoints_ += ((id, c))
    } catch {
      case e : Throwable => throw CheckpointException(s"""Could not create Checkpoint "$id""", Some(e))
    }
  }

  /**
    * Restores the current program state from a previously saved checkpoint.
    *
    * @param id The identifier to be used to find the checkpoint that is to be restored.
    */
  def restore(id : CheckpointIdentifier) : Unit = {
    Logger.debug(s"""Restoring to checkpoint "$id"""")
    StateManager.setRoot(checkpoints_.getOrElse(id, { throw CheckpointException(s"""Could not restore to checkpoint "$id": Not found""") }))
  }

  /**
    * List identifiers of all currently known checkpoints.
    *
    * @return The list of identifiers of all currently known checkpoints.
    */
  def listCheckpoints() = checkpoints_.keys
}