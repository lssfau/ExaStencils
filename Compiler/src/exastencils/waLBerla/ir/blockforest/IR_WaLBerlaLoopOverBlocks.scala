package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_LoopOverProcessLocalBlocks
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.parallelization.api.cuda.CUDA_PrepareHostCode.annotateBranch
import exastencils.parallelization.api.cuda.CUDA_PrepareHostCode.getCondWrapperValue
import exastencils.parallelization.ir._
import exastencils.util.NoDuplicateWrapper
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.field.IR_MultiDimWaLBerlaFieldAccess
import exastencils.waLBerla.ir.field._

object FindLoopOverDimensions extends QuietDefaultStrategy("Find loop over dimensions") {
  var loopOverDims : Option[IR_LoopOverDimensions] = None

  override def applyStandalone(node : Node) : Unit = {
    loopOverDims = None
    super.applyStandalone(node)
  }

  this += Transformation("..", {
    case loopOverDimensions : IR_LoopOverDimensions =>
      loopOverDims = Some(loopOverDimensions)
      loopOverDimensions
  })
}

/// IR_WaLBerlaLoopOverBlocks

object IR_WaLBerlaLoopOverBlocks {
  def apply(body : IR_Statement*) = new IR_WaLBerlaLoopOverBlocks(body.to[ListBuffer])
  def apply(body : IR_Statement, parallelization : IR_ParallelizationInfo) = new IR_WaLBerlaLoopOverBlocks(ListBuffer(body), parallelization)

  def defIt = IR_LoopOverFragments.defIt
  def block = IR_WaLBerlaBlockForest().iterator
}

// iterates through process-local blocks
case class IR_WaLBerlaLoopOverBlocks(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo(),
    var setupWaLBerlaFieldPointers : Boolean = true) extends IR_LoopOverProcessLocalBlocks {

  def expandSpecial(collector : IR_StackCollector) : Output[IR_Scope] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    // collect fields accessed per level in loop
    var fieldsAccessed = ListBuffer[IR_MultiDimWaLBerlaFieldAccess]()
    IR_WaLBerlaCollectAccessedFields.applyStandalone(body)
    fieldsAccessed ++= Duplicate(IR_WaLBerlaCollectAccessedFields.wbFieldAccesses)
      .groupBy(wbf => (wbf.name, wbf.fragIdx, wbf.level)).map(_._2.head) // distinctBy name, fragIdx and level

    // find out if block loop contains loop over dimensions and if it is executed (in parallel) on CPU/GPU
    FindLoopOverDimensions.applyStandalone(IR_Scope(body))
    val optLoopOverDims = FindLoopOverDimensions.loopOverDims
    val cpuExecution = NoDuplicateWrapper[IR_Expression](IR_BooleanConstant(true))
    val condWrapper = if (Knowledge.cuda_enabled && optLoopOverDims.isDefined && optLoopOverDims.get.parallelization.potentiallyParallel)
      NoDuplicateWrapper[IR_Expression](getCondWrapperValue(optLoopOverDims.get))
    else
      cpuExecution

    def getFieldPointer(fAcc : IR_MultiDimWaLBerlaFieldAccess) = IR_IV_WaLBerlaFieldData(fAcc.field, fAcc.slot, fAcc.fragIdx)

    // get data pointer to CPU/GPU memory and (additionally) fetch CPU field instance (used e.g. for variable-sized layouts)
    def getWaLBerlaFieldData(accesses : IR_MultiDimWaLBerlaFieldAccess*) : ListBuffer[IR_Statement] = {
      accesses.to[mutable.ListBuffer].flatMap(fAcc => {
        val fieldPointer = getFieldPointer(fAcc)

        val stmts = ListBuffer[IR_Statement]()

        // declare field data pointer
        stmts += fieldPointer.getDeclaration()
        if (Knowledge.cuda_enabled)
          stmts ++= annotateBranch(condWrapper, fieldPointer.initInBlockLoop(onGPU = false), fieldPointer.initInBlockLoop(onGPU = true))
        else
          stmts ++= fieldPointer.initInBlockLoop(onGPU = false)

        stmts
      })
    }

    import IR_WaLBerlaLoopOverBlocks.block

    // check if contained within a block loop (resolved or unresolved)
    val insideBlockLoop = collector.stack.exists {
      case _ : IR_LoopOverProcessLocalBlocks                                                       => true
      case _ @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == defIt.name => true
      case _                                                                                       => false
    }

    // array of process-local blocks
    val blockArray = IR_WaLBerlaGetBlocks()

    var compiledBody = ListBuffer[IR_Statement]()

    // setup loop body to support accesses to waLBerla data structures
    if (!insideBlockLoop) {
      // init block pointer in loop
      compiledBody += IR_VariableDeclaration(block, IR_ArrayAccess(blockArray, defIt))

      // get field data if necessary
      if (setupWaLBerlaFieldPointers)
        compiledBody ++= getWaLBerlaFieldData(fieldsAccessed : _*)

      // check if there are block loop variables to be added (i.e. declared and set)
      IR_WaLBerlaFindBlockLoopVariables.applyStandalone(IR_Scope(body))
      for (blockVar <- IR_WaLBerlaFindBlockLoopVariables.blockLoopVariables.distinct.sorted)
        compiledBody += blockVar.getDeclaration()
    }

    compiledBody ++= body

    if (!insideBlockLoop) {
      val upperBoundKnown = Knowledge.waLBerla_useGridFromExa
      val upperBound : IR_Expression = if (upperBoundKnown)
        Knowledge.domain_numFragmentsPerBlock
      else
        IR_Cast(IR_IntegerDatatype, IR_MemberFunctionCall(blockArray, "size"))

      // wrap around explicit block loop, if not already done
      val loop = IR_ForLoop(
        IR_VariableDeclaration(defIt, 0),
        IR_Lower(defIt, upperBound),
        IR_PreIncrement(defIt),
        compiledBody,
        parallelization)

      if (upperBoundKnown)
        loop.annotate("numLoopIterations", Knowledge.domain_numFragmentsPerBlock)

      IR_Scope(loop)
    } else {
      IR_Scope(compiledBody)
    }
  }
}

object IR_WaLBerlaResolveLoopOverBlocks extends DefaultStrategy("Resolve waLBerla LoopOverBlocks") {
  val collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverBlocks => loop.expandSpecial(collector)
  })
}
