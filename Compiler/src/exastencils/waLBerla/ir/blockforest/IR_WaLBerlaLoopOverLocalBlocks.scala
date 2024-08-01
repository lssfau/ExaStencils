package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_LoopOverProcessLocalBlocks
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.parallelization.api.cuda.CUDA_ExecutionBranching
import exastencils.parallelization.api.cuda.CUDA_PrepareHostCode.getTimeEstimation
import exastencils.parallelization.ir._
import exastencils.util.NoDuplicateWrapper
import exastencils.util.ir.IR_CollectFieldAccesses
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

object IR_WaLBerlaLoopOverLocalBlocks {
  def apply(body : IR_Statement*) = new IR_WaLBerlaLoopOverLocalBlocks(body.to[ListBuffer])
  def apply(body : IR_Statement, parallelization : IR_ParallelizationInfo) = new IR_WaLBerlaLoopOverLocalBlocks(ListBuffer(body), parallelization)

  def defIt = IR_LoopOverFragments.defIt
  def block = IR_WaLBerlaBlock()
}

// iterates through process-local blocks
case class IR_WaLBerlaLoopOverLocalBlocks(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_LoopOverProcessLocalBlocks with CUDA_ExecutionBranching {

  def expandSpecial(collector : IR_StackCollector) : Output[IR_Scope] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    // collect fields accessed per level in loop
    var fieldsAccessed = ListBuffer[IR_MultiDimWaLBerlaFieldAccess]()
    IR_WaLBerlaCollectAccessedFields.applyStandalone(body)
    fieldsAccessed ++= IR_WaLBerlaCollectAccessedFields.wbFieldAccesses.map(fAcc => Duplicate(fAcc))
      .groupBy(wbf => (wbf.name, wbf.fragIdx, wbf.level)).map(_._2.head) // distinctBy name, fragIdx and level

    // ensure consistency of data flow between wb and exa fields
    if (!Knowledge.waLBerla_useGridPartFromExa) {
      IR_CollectFieldAccesses.applyStandalone(body)

      if (IR_CollectFieldAccesses.fieldAccesses.exists(!_.field.isInstanceOf[IR_WaLBerlaField]))
        Logger.error("Exchange between ExaStencils and waLBerla fields is currently only available when both grids are identical (i.e. waLBerla_useGridPartFromExa = true).")
    }

    // find out if block loop contains loop over dimensions and if it is executed (in parallel) on CPU/GPU
    FindLoopOverDimensions.applyStandalone(IR_Scope(body))
    val optLoopOverDims = FindLoopOverDimensions.loopOverDims

    def getFieldPointer(fAcc : IR_MultiDimWaLBerlaFieldAccess) = IR_IV_WaLBerlaFieldData(fAcc.field, fAcc.slot, fAcc.fragIdx)

    // get data pointer to CPU/GPU memory and (additionally) fetch CPU field instance (used e.g. for variable-sized layouts)
    def getWaLBerlaFieldData(accesses : IR_MultiDimWaLBerlaFieldAccess*) : ListBuffer[IR_Statement] = {
      accesses.to[mutable.ListBuffer].flatMap(fAcc => {
        val fieldPointer = getFieldPointer(fAcc)

        val stmts = ListBuffer[IR_Statement]()

        // declare field data pointer
        stmts += fieldPointer.getDeclaration()
        if (Knowledge.cuda_enabled) {
          // check if it is possibly faster to execute kernel (with LoopOverDims) on CPU
          val estimatedFasterHostExec = if (optLoopOverDims.isDefined)
            getTimeEstimation(optLoopOverDims.get, host = true) <= getTimeEstimation(optLoopOverDims.get, host = false)
          else
            false

          stmts ++= getHostDeviceBranching(fieldPointer.initInBlockLoop(onGPU = false), fieldPointer.initInBlockLoop(onGPU = true),
            estimatedFasterHostExec)
        } else {
          stmts ++= fieldPointer.initInBlockLoop(onGPU = false)
        }

        stmts
      })
    }

    // check if contained within a block loop (resolved or unresolved)
    val insideBlockLoop = collector.stack.exists {
      case _ : IR_LoopOverProcessLocalBlocks                                                       => true
      case _ @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == defIt.name => true
      case _                                                                                       => false
    }

    var compiledBody = ListBuffer[IR_Statement]()

    // setup loop body to access field data from waLBerla data structures
    if (!insideBlockLoop)
      compiledBody ++= getWaLBerlaFieldData(fieldsAccessed : _*)

    compiledBody ++= body

    if (!insideBlockLoop)
      IR_Scope(IR_WaLBerlaLoopOverLocalBlockArray(compiledBody, parallelization))
    else
      IR_Scope(compiledBody)
  }
}

object IR_WaLBerlaResolveLoopOverLocalBlocks extends DefaultStrategy("Resolve waLBerla LoopOverLocalBlocks") {
  val collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverLocalBlocks => loop.expandSpecial(collector)
  })
}

object IR_WaLBerlaResolveLoopOverLocalBlockArray extends DefaultStrategy("Resolve waLBerla LoopOverLocalBlockArray") {
  this += Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverLocalBlockArray => loop.expandSpecial()
  })
}
