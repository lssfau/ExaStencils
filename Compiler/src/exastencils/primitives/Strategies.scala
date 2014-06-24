package exastencils.primitives

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.TreeMap
import exastencils.core._
import exastencils.util._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.strategies._
import exastencils.omp._
import exastencils.multiGrid._

object SetupFragment extends DefaultStrategy("Setting up fragment") {
  override def apply(node : Option[Node] = None) = {
    Fragment.setupNeighbors
    StateManager.findFirst[Globals]().get.functions += new SetupBuffers(FieldCollection.fields, Fragment.neighbors)
    super.apply(node)
  }

  this += new Transformation("Adding relevant functions to CommunicationFunctions", {
    case commFu : CommunicationFunctions =>
      if (Knowledge.useMPI && Knowledge.domain_canHaveRemoteNeighs)
        commFu.functions += new WaitForMPIRequestFunc
      if (Knowledge.domain_canHaveLocalNeighs)
        commFu.functions += new ConnectLocalElement()
      if (Knowledge.domain_canHaveRemoteNeighs)
        commFu.functions += new ConnectRemoteElement()
      for (field <- FieldCollection.fields) {
        Knowledge.comm_strategyFragment match {
          case 6  => commFu.functions += new ExchangeData_6(FieldSelection(field, "slot", -1), Fragment.neighbors)
          case 26 => commFu.functions += new ExchangeData_26(FieldSelection(field, "slot", -1), Fragment.neighbors)
        }
      }

      commFu
  })

  this += new Transformation("Adding external field transfer functions", {
    case multiGrid : MultiGridFunctions =>
      for (extField <- ExternalFieldCollection.fields) {
        multiGrid.functions += new GetFromExternalField(extField.targetField, extField)
        multiGrid.functions += new SetFromExternalField(extField.targetField, extField)
      }
      multiGrid
  })
}

object AddInternalVariables extends DefaultStrategy("Adding internal variables") {
  var declarationMap : TreeMap[String, VariableDeclarationStatement] = TreeMap()
  var ctorMap : TreeMap[String, Statement] = TreeMap()
  var dtorMap : TreeMap[String, Statement] = TreeMap()

  this += new Transformation("Collecting", {
    case mem : iv.InternalVariable => // TODO: don't overwrite for performance reasons
      declarationMap += (mem.resolveName -> mem.getDeclaration)
      if (mem.getCtor().isDefined)
        ctorMap += (mem.resolveName -> mem.getCtor().get)
      if (mem.getDtor().isDefined)
        dtorMap += (mem.resolveName -> mem.getDtor().get)
      mem
  })

  this += new Transformation("Adding to globals", {
    case globals : Globals =>
      for (decl <- declarationMap)
        globals.variables += decl._2
      globals
    case func : FunctionStatement if (("initGlobals" : Expression) == func.name) =>
      func.body ++= ctorMap.map(_._2)
      func
    case func : FunctionStatement if (("destroyGlobals" : Expression) == func.name) =>
      func.body ++= dtorMap.map(_._2)
      func
  })

  var bufferSizes : TreeMap[Expression, Int] = TreeMap()(Ordering.by(_.cpp))

  this += new Transformation("Collecting buffer sizes", {
    case buf : iv.TmpBuffer =>
      val size = SimplifyExpression.evalIntegral(buf.size).toInt
      val id = buf.resolveAccess(buf.resolveName, LoopOverFragments.defIt, new NullExpression, buf.field.index, buf.field.level, buf.neighIdx)
      bufferSizes += (id -> (size max bufferSizes.getOrElse(id, 0)))
      buf
  })

  this += new Transformation("Extending SetupBuffers function", {
    // FIXME: this kind of matching is awkward, I want trafos that don't return nodes
    case func : FunctionStatement if (("setupBuffers" : Expression) == func.name) =>
      func.body += new LoopOverFragments(-1, bufferSizes.map(buf => new AssignmentStatement(buf._1, s"new double[${buf._2}]") : Statement).to[ListBuffer]) with OMP_PotentiallyParallel
      func
  })
}

object ResolveLoopOverDimensions extends DefaultStrategy("Resolving LoopOverDimensions nodes") {
  this += new Transformation("Resolving", {
    case loop : LoopOverDimensions =>
      loop.expandSpecial
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
