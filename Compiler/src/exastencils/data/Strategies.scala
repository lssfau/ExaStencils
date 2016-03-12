package exastencils.data

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.cuda._
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
      for (extField <- ExternalFieldCollection.getSortedFields) {
        multiGrid.functions += new GetFromExternalField(extField.targetField, extField)
        multiGrid.functions += new SetFromExternalField(extField.targetField, extField)
      }
      multiGrid
  })
}

object LinearizeFieldAccesses extends DefaultStrategy("Linearizing FieldAccess nodes") {
  this += new Transformation("Linearizing", {
    case access : DirectFieldAccess =>
      access.linearize
    case access : ExternalFieldAccess =>
      access.linearize
    case access : TempBufferAccess =>
      access.linearize
    case access : ReductionDeviceDataAccess =>
      access.linearize
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

object ResolveLoopOverPointsInOneFragment extends DefaultStrategy("Resolving LoopOverPointsInOneFragment nodes") {
  this += new Transformation("Resolving", {
    case loop : LoopOverPointsInOneFragment =>
      loop.expandSpecial
  })
}

object ResolveLoopOverPoints extends DefaultStrategy("Resolving ResolveLoopOverPoints nodes") {
  val collector = new StackCollector
  this.register(collector)

  this += new Transformation("Resolving", {
    case loop : LoopOverPoints =>
      loop.expandSpecial(collector)
  })
}

object ResolveSlotOperationsStrategy extends DefaultStrategy("ResolveSlotOperations") {
  var collector = new StackCollector
  this.register(collector)

  this += new Transformation("SearchAndReplace", {
    case slotAccess : SlotAccess => slotAccess.expandSpecial

    case advanceSlot : AdvanceSlotStatement =>
      // check if already inside a fragment loop - if not wrap the expanded statement
      if (collector.stack.map(node => node match {
        case _ : LoopOverFragments => true
        case ForLoopStatement(VariableDeclarationStatement(_, it, _), _, _, _, _) if (LoopOverFragments.defIt == it) => true
        case _ => false
      }).fold(false)((a, b) => a || b))
        advanceSlot.expandSpecial
      else
        new LoopOverFragments(advanceSlot.expandSpecial) with OMP_PotentiallyParallel // TODO: parallelization will probably be quite slow -> SPL?
  })
}

// Note: Must run after ResolveLoopOverPointsInOneFragment
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

/// internal variables

object ResolveConstInternalVariables extends DefaultStrategy("Resolving constant internal variables") {
  override def apply(applyAtNode : Option[Node]) = {
    this.transaction()

    if (DomainCollection.domains.size <= 1)
      this.execute(new Transformation("Resolving IsValidForSubdomain", {
        case AssignmentStatement(_ : iv.IsValidForSubdomain, _, _) => NullStatement
        case _ : iv.IsValidForSubdomain                            => BooleanConstant(true)
      }))

    if (!Knowledge.mpi_enabled || Knowledge.mpi_numThreads <= 1)
      this.execute(new Transformation("Resolving NeighborIsRemote and NeighborRemoteRank", {
        case AssignmentStatement(_ : iv.NeighborIsRemote, _, _)   => NullStatement
        case _ : iv.NeighborIsRemote                              => BooleanConstant(false)
        case AssignmentStatement(_ : iv.NeighborRemoteRank, _, _) => NullStatement
        case _ : iv.NeighborRemoteRank                            => IntegerConstant(-1)
      }))

    if (Knowledge.domain_numFragmentsTotal <= 1 && !Knowledge.domain_rect_hasPeriodicity) {
      this.execute(new Transformation("Resolving NeighborIsValid", {
        case AssignmentStatement(_ : iv.NeighborIsValid, _, _) => NullStatement
        case _ : iv.NeighborIsValid                            => BooleanConstant(false)
      }))
    } else if (Knowledge.domain_rect_generate) {
      for (dim <- 0 until Knowledge.dimensionality)
        if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1 && !Knowledge.domain_rect_periodicAsVec(dim))
          this.execute(new Transformation(s"Resolving NeighborIsValid in dimension $dim", {
            case AssignmentStatement(niv : iv.NeighborIsValid, _, _) if (niv.neighIdx.isInstanceOf[IntegerConstant])
              && (Fragment.neighbors(niv.neighIdx.asInstanceOf[IntegerConstant].value.toInt).dir(dim) != 0) => NullStatement
            case niv : iv.NeighborIsValid if (niv.neighIdx.isInstanceOf[IntegerConstant])
              && (Fragment.neighbors(niv.neighIdx.asInstanceOf[IntegerConstant].value.toInt).dir(dim) != 0) => BooleanConstant(false)
          }))
    }

    if (Knowledge.domain_numFragmentsPerBlock <= 1 || Knowledge.comm_disableLocalCommSync) {
      this.execute(new Transformation("Resolving local synchronization", {
        case AssignmentStatement(_ : iv.LocalCommReady, _, _) => NullStatement
        case _ : iv.LocalCommReady                            => BooleanConstant(true)

        case AssignmentStatement(_ : iv.LocalCommDone, _, _)  => NullStatement
        case _ : iv.LocalCommDone                             => BooleanConstant(true)

        case FunctionCallExpression("waitForFlag", _)         => NullExpression
      }))
    }

    this.commit()
  }
}

object AddInternalVariables extends DefaultStrategy("Adding internal variables") {
  var declarationMap : HashMap[String, VariableDeclarationStatement] = HashMap()
  var ctorMap : HashMap[String, Statement] = HashMap()
  var dtorMap : HashMap[String, Statement] = HashMap()

  var bufferSizes : HashMap[String, Expression] = HashMap()
  var bufferAllocs : HashMap[String, Statement] = HashMap()
  var fieldAllocs : HashMap[String, Statement] = HashMap()

  var deviceBufferSizes : HashMap[String, Expression] = HashMap()
  var deviceBufferAllocs : HashMap[String, Statement] = HashMap()
  var deviceFieldAllocs : HashMap[String, Statement] = HashMap()

  var counter : Int = 0

  override def apply(node : Option[Node] = None) = {
    counter = 0
    for (map <- List(declarationMap, ctorMap, dtorMap, bufferSizes, bufferAllocs, fieldAllocs)) map.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    counter = 0
    for (map <- List(declarationMap, ctorMap, dtorMap, bufferSizes, bufferAllocs, fieldAllocs)) map.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collecting buffer sizes", {
    case buf : iv.TmpBuffer => {
      val id = buf.resolveAccess(buf.resolveName, LoopOverFragments.defIt, NullExpression, buf.field.index, buf.field.level, buf.neighIdx).prettyprint
      if (Knowledge.data_genVariableFieldSizes) {
        if (bufferSizes.contains(id))
          bufferSizes.get(id).get.asInstanceOf[MaximumExpression].args += Duplicate(buf.size)
        else
          bufferSizes += (id -> MaximumExpression(ListBuffer(Duplicate(buf.size))))
      } else {
        val size = SimplifyExpression.evalIntegral(buf.size).toLong
        bufferSizes += (id -> (size max bufferSizes.getOrElse(id, IntegerConstant(0)).asInstanceOf[IntegerConstant].v))
      }
      buf
    }

    case field : iv.FieldData => {
      val cleanedField = Duplicate(field)
      cleanedField.slot = "slot"
      cleanedField.fragmentIdx = LoopOverFragments.defIt

      var numDataPoints : Expression = (0 until field.field.fieldLayout.numDimsData).map(dim => field.field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
      var statements : ListBuffer[Statement] = ListBuffer()

      val newFieldData = Duplicate(cleanedField)
      newFieldData.slot = (if (field.field.numSlots > 1) "slot" else 0)

      var innerStmts : ListBuffer[Statement] =
        if (Knowledge.data_alignFieldPointers) {
          counter += 1
          ListBuffer(
            VariableDeclarationStatement(SpecialDatatype("ptrdiff_t"), s"vs_$counter",
              Some(Knowledge.simd_vectorSize * SizeOfExpression(RealDatatype))),
            AssignmentStatement(newFieldData.basePtr, Allocation(field.field.resolveDeclType, numDataPoints + Knowledge.simd_vectorSize - 1)),
            VariableDeclarationStatement(SpecialDatatype("ptrdiff_t"), s"offset_$counter",
              Some(((s"vs_$counter" - (CastExpression(SpecialDatatype("ptrdiff_t"), newFieldData.basePtr) Mod s"vs_$counter")) Mod s"vs_$counter") / SizeOfExpression(RealDatatype))),
            AssignmentStatement(newFieldData, newFieldData.basePtr + s"offset_$counter"))
        } else {
          ListBuffer(AssignmentStatement(newFieldData, Allocation(field.field.resolveDeclType, numDataPoints)))
        }

      if (field.field.numSlots > 1)
        statements += new ForLoopStatement(
          VariableDeclarationStatement(IntegerDatatype, "slot", Some(0)),
          LowerExpression("slot", field.field.numSlots),
          PreIncrementExpression("slot"),
          innerStmts)
      else
        statements ++= innerStmts

      fieldAllocs += (cleanedField.prettyprint() -> new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.field.domain.index), statements)) with OMP_PotentiallyParallel)

      field
    }

    case field : iv.FieldDeviceData => {
      val cleanedField = Duplicate(field)
      cleanedField.slot = "slot"
      cleanedField.fragmentIdx = LoopOverFragments.defIt

      var numDataPoints : Expression = (0 until field.field.fieldLayout.numDimsData).map(dim => field.field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
      var statements : ListBuffer[Statement] = ListBuffer()

      val newFieldData = Duplicate(cleanedField)
      newFieldData.slot = (if (field.field.numSlots > 1) "slot" else 0)

      var innerStmts = ListBuffer[Statement](
        CUDA_AllocateStatement(newFieldData, numDataPoints, field.field.resolveBaseDatatype))

      if (field.field.numSlots > 1)
        statements += new ForLoopStatement(
          VariableDeclarationStatement(IntegerDatatype, "slot", Some(0)),
          LowerExpression("slot", field.field.numSlots),
          PreIncrementExpression("slot"),
          innerStmts)
      else
        statements ++= innerStmts

      deviceFieldAllocs += (cleanedField.prettyprint() -> new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.field.domain.index), statements)) with OMP_PotentiallyParallel)

      field
    }

    case buf : iv.ReductionDeviceData => {
      val id = buf.resolveAccess(buf.resolveName, LoopOverFragments.defIt, NullExpression, NullExpression, NullExpression, NullExpression).prettyprint
      if (Knowledge.data_genVariableFieldSizes) {
        if (deviceBufferSizes.contains(id))
          deviceBufferSizes.get(id).get.asInstanceOf[MaximumExpression].args += Duplicate(buf.size)
        else
          deviceBufferSizes += (id -> MaximumExpression(ListBuffer(Duplicate(buf.size))))
      } else {
        val size = SimplifyExpression.evalIntegral(buf.size).toLong
        deviceBufferSizes += (id -> (size max bufferSizes.getOrElse(id, IntegerConstant(0)).asInstanceOf[IntegerConstant].v))
      }
      buf
    }
  })

  this += new Transformation("Updating temporary buffer allocations", {
    case buf : iv.TmpBuffer =>
      val id = buf.resolveAccess(buf.resolveName, LoopOverFragments.defIt, NullExpression, buf.field.index, buf.field.level, buf.neighIdx).prettyprint
      val size = bufferSizes(id)

      if (Knowledge.data_alignTmpBufferPointers) {
        counter += 1
        bufferAllocs += (id -> new LoopOverFragments(ListBuffer[Statement](
          VariableDeclarationStatement(SpecialDatatype("ptrdiff_t"), s"vs_$counter",
            Some(Knowledge.simd_vectorSize * SizeOfExpression(RealDatatype))),
          AssignmentStatement(buf.basePtr, Allocation(RealDatatype, size + Knowledge.simd_vectorSize - 1)),
          VariableDeclarationStatement(SpecialDatatype("ptrdiff_t"), s"offset_$counter",
            Some(((s"vs_$counter" - (CastExpression(SpecialDatatype("ptrdiff_t"), buf.basePtr) Mod s"vs_$counter")) Mod s"vs_$counter") / SizeOfExpression(RealDatatype))),
          AssignmentStatement(buf, buf.basePtr + s"offset_$counter"))) with OMP_PotentiallyParallel)
      } else {
        bufferAllocs += (id -> new LoopOverFragments(new AssignmentStatement(buf, Allocation(RealDatatype, size))) with OMP_PotentiallyParallel)
      }

      buf

    case buf : iv.ReductionDeviceData =>
      val id = buf.resolveAccess(buf.resolveName, LoopOverFragments.defIt, NullExpression, NullExpression, NullExpression, NullExpression).prettyprint
      val size = deviceBufferSizes(id)

      deviceBufferAllocs += (id -> new LoopOverFragments(CUDA_AllocateStatement(buf, size, RealDatatype /*FIXME*/ )) with OMP_PotentiallyParallel)

      buf
  })

  this += new Transformation("Extending SetupBuffers function", {
    case func @ FunctionStatement(_, "setupBuffers", _, _, _, _, _) => {
      if (Knowledge.experimental_useLevelIndepFcts) {
        val s = new DefaultStrategy("Replacing level specifications")
        s += new Transformation("Search and replace", {
          case StringLiteral("level")     => Knowledge.maxLevel : Expression
          case VariableAccess("level", _) => Knowledge.maxLevel : Expression
        })
        for (buf <- bufferSizes)
          s.applyStandalone(buf._2)
      }

      for (genericAlloc <- bufferAllocs.toSeq.sortBy(_._1) ++ fieldAllocs.toSeq.sortBy(_._1) ++ deviceFieldAllocs.toSeq.sortBy(_._1) ++ deviceBufferAllocs.toSeq.sortBy(_._1))
        if ("MSVC" == Knowledge.targetCompiler /*&& Knowledge.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
          func.body += new Scope(genericAlloc._2)
        else
          func.body += genericAlloc._2

      func
    }
  })

  this += new Transformation("Collecting", {
    case mem : iv.InternalVariable =>
      mem.registerIV(declarationMap, ctorMap, dtorMap)
      mem
  })

  this += new Transformation("Adding to globals", {
    case globals : Globals =>
      globals.variables ++= declarationMap.toSeq.sortBy(_._1).map(_._2)
      globals
    case func : FunctionStatement if ("initGlobals" == func.name) =>
      if ("MSVC" == Knowledge.targetCompiler /*&& Knowledge.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
        func.body ++= ctorMap.toSeq.sortBy(_._1).map(s => new Scope(s._2))
      else
        func.body ++= ctorMap.toSeq.sortBy(_._1).map(_._2)
      func
    case func : FunctionStatement if ("destroyGlobals" == func.name) =>
      if ("MSVC" == Knowledge.targetCompiler /*&& Knowledge.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
        func.body ++= dtorMap.toSeq.sortBy(_._1).map(s => new Scope(s._2))
      else
        func.body ++= dtorMap.toSeq.sortBy(_._1).map(_._2)
      func
  })
}
