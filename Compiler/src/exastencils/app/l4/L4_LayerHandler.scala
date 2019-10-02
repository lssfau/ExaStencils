package exastencils.app.l4

import exastencils.app.LayerHandler
import exastencils.applications.l4.L4_AddDefaultApplication
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
import exastencils.grid.l4._
import exastencils.interfacing.l4.L4_ExternalFieldCollection
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.layoutTransformation.l4.L4_AddSoAtoAoSTransformation
import exastencils.logger.Logger
import exastencils.operator.l4._
import exastencils.optimization.l4.L4_GeneralSimplify
import exastencils.parsers.l4._
import exastencils.prettyprinting.Indenter
import exastencils.solver.l4._
import exastencils.timing.l4.L4_ResolveTimerFunctions
import exastencils.util.l4._

/// L4_LayerHandler

trait L4_LayerHandler extends LayerHandler

/// L4_DummyLayerHandler

object L4_DummyLayerHandler extends L4_LayerHandler {
  def initialize() : Unit = {}
  def handle() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

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
    L4_FieldCombinationCollection
  }

  override def shutdown() : Unit = {
    L4_KnowledgeContainer.clear()
  }

  override def print() : Unit = {
    if (Settings.getDebugL4file.nonEmpty) {
      val outFile = new java.io.FileWriter(Settings.getDebugL4file)
      outFile.write(Indenter.addIndentations(ExaRootNode.l4_root.prettyprint()))
      outFile.close()
    }
  }

  override def handle() : Unit = {
    if (Settings.timeStrategies) StrategyTimer.startTiming("Handling Layer 4")

    if (ExaRootNode.l4_root.nodes.nonEmpty) {
      L4_WrapFieldFieldConvolutions.apply()
      L4_AddLoopsToFieldAssignments.apply()
      L4_AddLoopsToLocalSolve.apply()
      L4_AddCommunicationToLoops.apply()
      L4_AdaptFieldLayoutsForComm.apply()
    }

    ExaRootNode.mergeL4(L4_Root(Settings.getL4file.map(L4_Parser.parseFile(_) : L4_Node)))
    ExaRootNode.l4_root.flatten()

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
    if (Knowledge.l4_genSoA2AoSTransformation)
      L4_AddSoAtoAoSTransformation.apply()

    print()
    val oldL4Code = ExaRootNode.l4_root.prettyprint()

    // re-parse the code to check for errors - also clear knowledge collections
    L4_KnowledgeContainer.clear()

    val l4FileName = if (Settings.getDebugL4file.nonEmpty) Settings.getDebugL4file else "debugLayer4"
    ExaRootNode.l4_root = L4_Parser.parse(oldL4Code, l4FileName)
    ExaRootNode.l4_root.flatten()

    if (ExaRootNode.l4_root.nodes.nonEmpty) {
      L4_ProcessInlineKnowledge.apply()

      if (Knowledge.l4_genDefaultApplication)
        L4_AddDefaultApplication.apply()

      L4_Validation.apply()

      if (Settings.timeStrategies) StrategyTimer.stopTiming("Handling Layer 4")

      L4_UnifyGlobalSections.apply()

      // go to IR

      L4_ResolveColorLoops.apply()

      // pre-process level specifications in declarations
      L4_ResolveLevelSpecifications.apply()

      L4_ResolveFunctionInstantiations.apply()
      L4_UnfoldFunctionDeclarations.apply()
      L4_ProcessFunctionDeclarations.apply()

      L4_UnfoldKnowledgeDeclarations.apply()
      L4_UnfoldLeveledExpressionDeclarations.apply()
      L4_UnfoldLeveledVariableDeclarations.apply()

      // resolve current, etc.
      L4_ResolveRelativeLevels.apply()

      L4_ResolveLevelScopes.apply()

      L4_PrepareDeclarations.apply()

      L4_InlineDeclaredExpressions.apply()
      L4_PrepareAccesses.apply()
      L4_ResolveVariableAccesses.apply()

      L4_ResolveSpecialConstants.apply()
      L4_ResolveFrozenFields.apply()
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
      L4_ResolveNativeFunctions.apply()
      L4_ResolvePrintFunctions.apply()
      L4_ResolveBuildStringFunctions.apply()
      L4_ResolveKnowledgeParameterAccess.apply()

      L4_GeneralSimplify.doUntilDone()

      L4_ProcessKnowledgeDeclarations.apply()

      if (Knowledge.l4_genSepLayoutsPerField)
        L4_DuplicateFieldLayoutsForFields.apply()

      // after L4_ResolveFieldAccesses
      L4_ResolveReadFieldFunctions.apply()
      L4_ResolvePrintFieldFunctions.apply()

      L4_ResolveBoundaryHandlingFunctions.apply()

      L4_ResolveStencilComponentAccesses.apply()
      L4_ResolveStencilFieldComponentAccesses.apply()
    }

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
