package exastencils.waLBerla.ir.refinement

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaInitBlockForest
import exastencils.waLBerla.ir.grid.IR_WaLBerlaAABB
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaPlainFunction
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_SetupBlockForest
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

case class IR_WaLBerlaInitNonuniformBlockForest() extends IR_WaLBerlaInitBlockForest {
  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    // check for errors
    checkErrors()

    var body : ListBuffer[IR_Statement] = ListBuffer()

    // init domain aabb
    body += IR_VariableDeclaration(domainAABB, IR_FunctionCall(IR_ExternalFunctionReference("math::AABB"), domainAABBLower ++ domainAABBUpper : _*))

    // get user-defined aabb refinement selection from L4 and add to 'AABBRefinementSelection' object
    val aabbRefinementSelection = IR_VariableAccess("aabbRefinementSelection", IR_SpecialDatatype("blockforest::AABBRefinementSelection"))
    body += IR_VariableDeclaration(aabbRefinementSelection)

    for (refinementSelection <- IR_WaLBerlaRefinementSelectionCollection.objects) {
      val refinementAABB = refinementSelection.aabb
      val lower = getLowerBoundsAABB(refinementAABB)
      val upper = getUpperBoundsAABB(refinementAABB)

      val aabb = IR_VariableAccess(s"aabb_${refinementSelection.name}", IR_WaLBerlaAABB.datatype)
      body += IR_VariableDeclaration(aabb, IR_FunctionCall(IR_ExternalFunctionReference("math::AABB"), lower ++ upper : _*))
      body += IR_MemberFunctionCall(aabbRefinementSelection, "addAABB", aabb, refinementSelection.refinementTargetLevel)
    }

    // add aabb refinement selections
    val refinementSelectionFunctions = IR_VariableAccess("refinementSelectionFunctions", IR_SpecialDatatype("blockforest::RefinementSelectionFunctions"))
    body += IR_VariableDeclaration(refinementSelectionFunctions)
    body += IR_MemberFunctionCall(refinementSelectionFunctions, "add", aabbRefinementSelection)

    // declare SetupBlockForest
    val setupForest = IR_VariableAccess("setupForest", WB_SetupBlockForest)
    body += IR_VariableDeclaration(setupForest)

    // register callbacks to SetupBlockforest: refinement selection, exclusion, workload & memory assignment
    body += IR_MemberFunctionCall(setupForest, "addRefinementSelectionFunction", refinementSelectionFunctions)
    body += IR_MemberFunctionCall(setupForest, "addWorkloadMemorySUIDAssignmentFunction", IR_WaLBerlaWorkloadAndMemoryAssignment().name)
    if (Knowledge.dimensionality == 2)
      body += IR_MemberFunctionCall(setupForest, "addBlockExclusionFunction", IR_WaLBerlaRefinementExclusion2D().name)

    // init SetupBlockForest
    body += IR_MemberFunctionCall(setupForest, "init",
      domainAABB,
      wbBlocks(0), wbBlocks(1), wbBlocks(2),
      periodicity(0), periodicity(1), periodicity(2))

    // distribute blocks to processes
    // TODO: integrate...

    // init BlockForest
    val blockForest = IR_VariableAccess("blockForest", IR_SpecialDatatype("auto"))
    body += IR_VariableDeclaration(blockForest,
      IR_WaLBerlaUtil.make_shared("BlockForest",
        IR_MemberFunctionCallArrow(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "rank"),
        setupForest,
        /* keepGlobalBlockInformation */ false)) // TODO: make (knowledge?) parameter

    // init structured blockforest

    val structuredBlockForest = IR_VariableAccess("structuredForest", datatype)
    body += IR_VariableDeclaration(structuredBlockForest)
    body += IR_Assignment(structuredBlockForest,
      IR_WaLBerlaUtil.make_shared("StructuredBlockForest",
        blockForest, numCellsBlock(0), numCellsBlock(1), numCellsBlock(2)))

    // create cell bounding boxes
    body += IR_MemberFunctionCallArrow(structuredBlockForest, "createCellBoundingBoxes")

    IR_WaLBerlaPlainFunction(name, datatype, ListBuffer(), body)
  }
  override def name : String = "createNonuniformBlockforest"
}
