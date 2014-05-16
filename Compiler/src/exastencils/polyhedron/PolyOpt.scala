package exastencils.polyhedron

import exastencils.core.Logger
import exastencils.core.StateManager
import exastencils.core.StateManager.History.TransactionToken
import exastencils.datastructures.Node
import exastencils.datastructures.Strategy
import exastencils.datastructures.Transformation

object PolyOpt extends Strategy("Polyhedral optimizations") {

  override def apply(hackedApplyAt : Option[Node] = None, hackedToken : Option[TransactionToken] = None) : Unit = {

    val token : TransactionToken = hackedToken.getOrElse(StateManager.transaction(this))

    StateManager.register(Extractor)
    transformations_ += new Transformation("extract model", PartialFunction.empty)
    super.apply(hackedApplyAt, Some(token))
    transformations_.clear()
    StateManager.unregister(Extractor)

    Logger.debug("    SCoPs: " + Extractor.scops.size)
    Logger.debug("    trash: " + Extractor.trash.size)
    
//    for (scop <- Extractor.scops) {
//      
//    }

    if (hackedToken.isEmpty)
      StateManager.commit(token)
  }
}
