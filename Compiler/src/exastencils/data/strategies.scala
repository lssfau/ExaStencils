package exastencils.data

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.multiGrid._

object SetupDataStructures extends DefaultStrategy("Setting up fragment") {
  override def apply(node : Option[Node] = None) = {
    Fragment.setupNeighbors
    StateManager.findFirst[Globals]().get.functions += new SetupBuffers(FieldCollection.fields, Fragment.neighbors)
    super.apply(node)
  }

  this += new Transformation("Adding external field transfer functions", {
    case multiGrid : MultiGridFunctions =>
      for (extField <- ExternalFieldCollection.fields) {
        multiGrid.functions += new GetFromExternalField(extField.targetField, extField)
        multiGrid.functions += new SetFromExternalField(extField.targetField, extField)
      }
      multiGrid
  })
}

object LinearizeFieldAccesses extends DefaultStrategy("Linearizing FieldAccess nodes") {
  this += new Transformation("Linearizing", {
    case loop : DirectFieldAccess =>
      loop.linearize
    case loop : FieldAccess =>
      loop.linearize
    case loop : ExternalFieldAccess =>
      loop.linearize
  })
}

object ResolveIndexOffsets extends DefaultStrategy("Resolving OffsetIndex nodes") {
  this += new Transformation("Resolving", {
    case index : OffsetIndex =>
      index.expandSpecial
  })
}

object ResolveLoopOverDimensions extends DefaultStrategy("Resolving LoopOverDimensions nodes") {
  this += new Transformation("Resolving", {
    case loop : LoopOverDimensions =>
      loop.expandSpecial
  })
}
