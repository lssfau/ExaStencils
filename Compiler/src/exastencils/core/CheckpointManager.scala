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