package exastencils.data

import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
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

object ResolveFieldAccess extends DefaultStrategy("Resolving FieldAccess nodes") {
  this += new Transformation("Resolving", {
    case loop : FieldAccess =>
      loop.expandSpecial
  })
}

object AddInternalVariables extends DefaultStrategy("Adding internal variables") {
  var declarationMap : HashMap[String, VariableDeclarationStatement] = HashMap()
  var ctorMap : HashMap[String, Statement] = HashMap()
  var dtorMap : HashMap[String, Statement] = HashMap()

  this += new Transformation("Collecting", {
    case mem : iv.InternalVariable => // TODO: don't overwrite for performance reasons
      mem.registerIV(declarationMap, ctorMap, dtorMap)
      mem
  })

  this += new Transformation("Adding to globals", {
    case globals : Globals =>
      globals.variables ++= declarationMap.toSeq.sortBy(_._1).map(_._2)
      globals
    case func : FunctionStatement if ("initGlobals" == func.name) =>
      func.body ++= ctorMap.toSeq.sortBy(_._1).map(_._2)
      func
    case func : FunctionStatement if ("destroyGlobals" == func.name) =>
      func.body ++= dtorMap.toSeq.sortBy(_._1).map(_._2)
      func
  })

  // add allocation stuff
  // TODO: restructure

  var bufferSizes : TreeMap[Expression, Expression] = TreeMap()(Ordering.by(_.prettyprint))
  var bufferAllocs : TreeMap[Expression, Statement] = TreeMap()(Ordering.by(_.prettyprint))
  var fieldAllocs : TreeMap[Expression, Statement] = TreeMap()(Ordering.by(_.prettyprint))

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
    case field : iv.FieldData => {
      val cleanedField = Duplicate(field)
      cleanedField.slot = "slot"
      cleanedField.fragmentIdx = LoopOverFragments.defIt

      var numDataPoints : Expression = field.field.fieldLayout.layoutsPerDim.map(l => l.total).reduceLeft(_ * _) * field.field.dataType.resolveFlattendSize
      var statements : ListBuffer[Statement] = ListBuffer()

      val newFieldData = Duplicate(cleanedField)
      newFieldData.slot = (if (field.field.numSlots > 1) "slot" else 0)

      var innerStmts : ListBuffer[Statement] =
        if (Knowledge.data_alignFieldPointers) {
          ListBuffer(
            VariableDeclarationStatement(SpecialDatatype("ptrdiff_t"), "vs",
              Some(Knowledge.simd_vectorSize * SizeOfExpression(RealDatatype()))),
            AssignmentStatement(newFieldData.basePtr, Allocation(field.field.dataType.resolveUnderlyingDatatype, numDataPoints + Knowledge.simd_vectorSize - 1)),
            VariableDeclarationStatement(SpecialDatatype("ptrdiff_t"), "offset",
              Some((("vs" - (CastExpression(SpecialDatatype("ptrdiff_t"), newFieldData.basePtr) Mod "vs")) Mod "vs") / SizeOfExpression(RealDatatype()))),
            AssignmentStatement(newFieldData, newFieldData.basePtr + "offset"))
        } else {
          ListBuffer(AssignmentStatement(newFieldData, Allocation(field.field.dataType.resolveUnderlyingDatatype, numDataPoints)))
        }

      if (field.field.numSlots > 1)
        statements += new ForLoopStatement(
          VariableDeclarationStatement(new IntegerDatatype, "slot", Some(0)),
          LowerExpression("slot", field.field.numSlots),
          PreIncrementExpression("slot"),
          innerStmts)
      else
        statements ++= innerStmts

      fieldAllocs += (cleanedField -> new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.field.domain.index), statements)) with OMP_PotentiallyParallel)
      field
    }
  })

  this += new Transformation("Updating temporary buffer allocations", {
    case buf : iv.TmpBuffer =>
      val id = buf.resolveAccess(buf.resolveName, LoopOverFragments.defIt, NullExpression, buf.field.index, buf.field.level, buf.neighIdx)
      val size = bufferSizes(id)

      if (Knowledge.data_alignFieldPointers) {
        bufferAllocs += (id -> new LoopOverFragments(ListBuffer[Statement](
          VariableDeclarationStatement(SpecialDatatype("ptrdiff_t"), "vs",
            Some(Knowledge.simd_vectorSize * SizeOfExpression(RealDatatype()))),
          AssignmentStatement(buf.basePtr, Allocation(new RealDatatype, size + Knowledge.simd_vectorSize - 1)),
          VariableDeclarationStatement(SpecialDatatype("ptrdiff_t"), "offset",
            Some((("vs" - (CastExpression(SpecialDatatype("ptrdiff_t"), buf.basePtr) Mod "vs")) Mod "vs") / SizeOfExpression(RealDatatype()))),
          AssignmentStatement(buf, buf.basePtr + "offset"))) with OMP_PotentiallyParallel)
      } else {
        bufferAllocs += (id -> new LoopOverFragments(new AssignmentStatement(buf, Allocation(new RealDatatype, size))) with OMP_PotentiallyParallel)
      }

      buf
  })

  this += new Transformation("Extending SetupBuffers function", {
    case func @ FunctionStatement(_, "setupBuffers", _, _) => {
      if (Knowledge.comm_useLevelIndependentFcts) {
        val s = new DefaultStrategy("Replacing level specifications")
        s += new Transformation("Search and replace", {
          case StringConstant("level")    => Knowledge.maxLevel : Expression
          case VariableAccess("level", _) => Knowledge.maxLevel : Expression
        })
        for (buf <- bufferSizes)
          s.applyStandalone(buf._2)
      }

      for (bufferAlloc <- bufferAllocs)
        func.body += bufferAlloc._2

      for (fieldAlloc <- fieldAllocs)
        func.body += fieldAlloc._2

      func
    }
  })
}
