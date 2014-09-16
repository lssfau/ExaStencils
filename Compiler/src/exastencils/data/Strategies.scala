package exastencils.data

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.multiGrid._
import exastencils.omp._
import exastencils.util._

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

object ResolveLoopOverPoints extends DefaultStrategy("Resolving ResolveLoopOverPoints nodes") {
  val collector = new StackCollector

  override def apply(node : Option[Node] = None) : Unit = {
    StateManager.register(collector)
    super.apply(node)
    StateManager.unregister(collector)
  }

  this += new Transformation("Resolving", {
    case loop : LoopOverPoints =>
      loop.expandSpecial(collector)
  })
}

object ResolveSlotOperationsStrategy extends DefaultStrategy("ResolveSlotOperations") {
  this += new Transformation("SearchAndReplace", {
    case slotAccess : SlotAccess   => slotAccess.expandSpecial
    case advanceSlot : AdvanceSlot => advanceSlot.expandSpecial
  })
}

object ResolveContractingLoop extends DefaultStrategy("Resolving ContractingLoop nodes") {
  this += new Transformation("Resolving", {
    case loop : ContractingLoop =>
      loop.expandSpecial
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

  var bufferSizes : TreeMap[Expression, Expression] = TreeMap()(Ordering.by(_.cpp))

  this += new Transformation("Collecting buffer sizes", {
    case buf : iv.TmpBuffer =>
      val id = buf.resolveAccess(buf.resolveName, LoopOverFragments.defIt, NullExpression, buf.field.index, buf.field.level, buf.neighIdx)
      if (Knowledge.comm_useLevelIndependentFcts) {
        if (bufferSizes.contains(id))
          bufferSizes.get(id).get.asInstanceOf[MaximumExpression].args += Duplicate(buf.size)
        else
          bufferSizes += (id -> MaximumExpression(ListBuffer(Duplicate(buf.size))))
      } else {
        val size = SimplifyExpression.evalIntegral(buf.size).toLong
        bufferSizes += (id -> (size max bufferSizes.getOrElse(id, IntegerConstant(0)).asInstanceOf[IntegerConstant].v))
      }
      buf
  })

  this += new Transformation("Extending SetupBuffers function", {
    // FIXME: this kind of matching is awkward, I want trafos that don't return nodes
    case func : FunctionStatement if (("setupBuffers" : Expression) == func.name) => {
      if (Knowledge.comm_useLevelIndependentFcts) {
        val s = new DefaultStrategy("Replacing level specifications")
        s += new Transformation("Search and replace", {
          case StringConstant("level")    => Knowledge.maxLevel : Expression
          case VariableAccess("level", _) => Knowledge.maxLevel : Expression
        })
        for (buf <- bufferSizes)
          s.applyStandalone(buf._2)
      }

      func.body += new LoopOverFragments(bufferSizes.map(buf => new AssignmentStatement(buf._1, "new double[" ~ buf._2 ~ "]") : Statement).to[ListBuffer]) with OMP_PotentiallyParallel
      func
    }
  })
}
