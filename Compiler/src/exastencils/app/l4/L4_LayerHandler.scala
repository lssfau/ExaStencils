package exastencils.app.l4

import exastencils.app.LayerHandler
import exastencils.base.ExaRootNode
import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.boundary.ir.L4_ResolveBoundaryHandlingFunctions
import exastencils.communication.l4._
import exastencils.config._
import exastencils.datastructures.StrategyTimer
import exastencils.domain.l4.L4_DomainCollection
import exastencils.field.ir.IR_AddPaddingToFieldLayouts
import exastencils.field.l4._
import exastencils.globals.l4.L4_AddDefaultApplication
import exastencils.grid.l4._
import exastencils.hack.l4.HACK_L4_ResolveNativeFunctions
import exastencils.interfacing.l4.L4_ExternalFieldCollection
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.logger.Logger
import exastencils.operator.l4._
import exastencils.optimization.l4.L4_GeneralSimplify
import exastencils.parsers.InputReader
import exastencils.parsers.l4._
import exastencils.prettyprinting.Indenter
import exastencils.solver.l4._
import exastencils.timing.l4.L4_ResolveTimerFunctions
import exastencils.util.l4._

/// L4_LayerHandler

trait L4_LayerHandler extends LayerHandler

/// L4_DefaultLayerHandler

object L4_DefaultLayerHandler extends L4_LayerHandler {
  override def initialize() : Unit = {
    // activate default knowledge collections

    L4_DomainCollection
    L4_FieldLayoutCollection
    L4_FieldCollection
    L4_StencilCollection
    L4_StencilFieldCollection
    L4_VirtualFieldCollection
    L4_ExternalFieldCollection
    L4_EquationCollection
  }

  override def shutdown() : Unit = {
    L4_KnowledgeContainer.clear()
  }

  override def handle() : Unit = {
    if (Settings.timeStrategies) StrategyTimer.startTiming("Handling Layer 4")

    if (Knowledge.experimental_layerExtension) {
      L4_IntroduceSlots.apply()
      L4_WrapFieldFieldConvolutions.apply()
      L4_AddLoopsToFieldAssignments.apply()
      L4_AddLoopsToLocalSolve.apply()
      L4_AddCommunicationToLoops.apply()
      L4_AdaptFieldLayoutsForComm.apply()

      if (Settings.inputFromJson)
        ExaRootNode.mergeL4(L4_Parser.parseFile(InputReader.layer4))
      else
        ExaRootNode.mergeL4(L4_Root(Settings.getL4file.map(L4_Parser.parseFile(_) : L4_Node)))

      if (true) {
        L4_UnresolveOperatorTimesField.apply()
        L4_UnresolveFieldFieldConvolutions.apply()
        L4_UnresolveStencilAccesses.apply()
        L4_UnresolveStencilFieldAccesses.apply()
        L4_UnresolveFieldAccesses.apply()
        // FIXME: transform back to declarations and re-fold
        L4_ReplaceLevelsInFunctionDecls.apply()
        L4_CombineLeveledFunctionDecls.apply()
        // L4_GenerateLeveledKnowledgeDecls.apply()
      }

      // re-print the merged L4 state
      val repFileName = { val tmp = Settings.getL4file.head.split('.'); tmp.dropRight(1).mkString(".") + "_rep." + tmp.last }
      val l4_printed = ExaRootNode.l4_root.prettyprint()

      val outFile = new java.io.FileWriter(repFileName)
      outFile.write(Indenter.addIndentations(l4_printed))
      outFile.close()

      // re-parse the file to check for errors - also clear knowledge collections
      L4_KnowledgeContainer.clear()

      ExaRootNode.l4_root = L4_Parser.parseFile(repFileName)
    } else {
      if (Settings.inputFromJson)
        ExaRootNode.l4_root = L4_Parser.parseFile(InputReader.layer4)
      else
        ExaRootNode.l4_root = L4_Root(Settings.getL4file.map(L4_Parser.parseFile(_) : L4_Node))
      ExaRootNode.l4_root.flatten()
    }

    if (Knowledge.l4_genDefaultApplication)
      L4_AddDefaultApplication.apply()

    L4_Validation.apply()

    if (Settings.timeStrategies) StrategyTimer.stopTiming("Handling Layer 4")

    L4_UnifyGlobalSections.apply()

    // go to IR

    // pre-process level specifications in declarations
    L4_ResolveLevelSpecifications.apply()

    L4_ResolveFunctionInstantiations.apply()
    L4_UnfoldFunctionDeclarations.apply()
    L4_ProcessFunctionDeclarations.apply()

    L4_UnfoldKnowledgeDeclarations.apply()
    L4_UnfoldLeveledVariableDeclarations.apply()

    // resolve current, etc.
    L4_ResolveRelativeLevels.apply()

    L4_PrepareDeclarations.apply()

    L4_PrepareAccesses.apply()
    L4_ResolveVariableAccesses.apply()

    L4_ResolveLevelScopes.apply()

    L4_ResolveSpecialConstants.apply()
    L4_ResolveDslFunctionReferences.apply()
    L4_ResolveEvaluateFunctions.apply()
    L4_ResolveIntegrateFunctions.apply()
    L4_ResolveMathFunctions.apply()
    L4_ResolveTimerFunctions.apply()

    var matches = 0
    do {
      matches = 0
      matches += L4_ProcessDeclarations.applyAndCountMatches()
      matches += L4_ResolveAccesses.applyAndCountMatches()

      if (Knowledge.experimental_l4_resolveVirtualFields) {
        // integrate before evaluate -> might be nested
        L4_ResolveIntegrateOnGrid.apply()
        matches += (if (L4_ResolveIntegrateOnGrid.results.isEmpty) 0 else L4_ResolveIntegrateOnGrid.results.last._2.matches)

        L4_ResolveEvaluateOnGrid.apply()
        matches += (if (L4_ResolveEvaluateOnGrid.results.isEmpty) 0 else L4_ResolveEvaluateOnGrid.results.last._2.matches)
      }
    } while (matches > 0)

    if (ExaRootNode.l4_root.nodes.exists(_.isInstanceOf[L4_KnowledgeDecl])) {
      val filtered = ExaRootNode.l4_root.nodes.filter(_.isInstanceOf[L4_KnowledgeDecl])
      Logger.warn(s"L4 root has ${ filtered.length } unprocessed declaration nodes remaining:")
      filtered.foreach(Logger.warn(_))
    }

    L4_ResolveVariableAccesses.apply()
    L4_ResolveStencilFunctions.apply()
    L4_ResolveLoopItAccesses.apply()
    HACK_L4_ResolveNativeFunctions.apply()
    L4_ResolvePrintFunctions.apply()
    L4_ResolveBuildStringFunctions.apply()
    L4_ResolveKnowledgeParameterAccess.apply()

    L4_GeneralSimplify.doUntilDone()

    L4_ProcessKnowledgeDeclarations.apply()

    if (Knowledge.l4_genSepLayoutsPerField)
      L4_DuplicateFieldLayoutsForFields.apply()

    // after L4_ResolveFieldAccesses
    L4_ResolvePrintFieldFunctions.apply()

    L4_ResolveBoundaryHandlingFunctions.apply()

    L4_ResolveStencilComponentAccesses.apply()
    L4_ResolveStencilFieldComponentAccesses.apply()

    if (Settings.timeStrategies) StrategyTimer.startTiming("Progressing from L4 to IR")

    // progress knowledge to IR
    L4_KnowledgeContainer.progress()

    //L4_ProgressKnowledge.apply()

    if (Knowledge.data_alignFieldPointers)
      IR_AddPaddingToFieldLayouts.apply()

    ExaRootNode.progressToIR()

    if (Settings.timeStrategies) StrategyTimer.stopTiming("Progressing from L4 to IR")
  }
}