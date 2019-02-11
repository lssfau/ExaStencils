package exastencils.app.l1

import exastencils.app.LayerHandler
import exastencils.base.ExaRootNode
import exastencils.base.l1._
import exastencils.baseExt.l1.L1_UnifyGlobalSections
import exastencils.config._
import exastencils.datastructures.StrategyTimer
import exastencils.discretization.l1._
import exastencils.domain.l1.L1_DomainCollection
import exastencils.field.l1._
import exastencils.knowledge.l1.L1_KnowledgeContainer._
import exastencils.knowledge.l1._
import exastencils.operator.l1.L1_OperatorCollection
import exastencils.parsers.l1.L1_Parser
import exastencils.prettyprinting.Indenter
import exastencils.solver.l1._
import exastencils.util.l1._

/// L1_LayerHandler

trait L1_LayerHandler extends LayerHandler

/// L1_DummyLayerHandler

object L1_DummyLayerHandler extends L1_LayerHandler {
  def initialize() : Unit = {}
  def handle() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

/// L1_DefaultLayerHandler

object L1_DefaultLayerHandler extends L1_LayerHandler {
  override def initialize() : Unit = {
    // activate default knowledge collections

    L1_DomainCollection
    L1_FieldCollection
    L1_OperatorCollection
    L1_EquationCollection
    L1_FieldCombinationCollection
  }

  override def shutdown() : Unit = {
    L1_KnowledgeContainer.clear()
  }

  override def print() : Unit = {
    if (Settings.getDebugL1file.nonEmpty) {
      val outFile = new java.io.FileWriter(Settings.getDebugL1file)
      outFile.write(Indenter.addIndentations(ExaRootNode.l1_root.prettyprint()))
      outFile.close()
    }
  }

  override def handle() : Unit = {
    if (Settings.timeStrategies) StrategyTimer.startTiming("Handling Layer 1")

    // add L1 code here
    ExaRootNode.l1_root = L1_Root(Settings.getL1file.map(L1_Parser.parseFile(_) : L1_Node))
    ExaRootNode.l1_root.flatten()
    print()

    if (ExaRootNode.l1_root.nodes.nonEmpty) {
      L1_ProcessInlineKnowledge.apply()

      L1_UnifyGlobalSections.apply()

      // pre-process level specifications in declarations
      L1_ResolveLevelSpecifications.apply()

      L1_UnfoldKnowledgeDeclarations.apply()
      L1_UnfoldLeveledVariableDeclarations.apply()

      // resolve current, etc.
      L1_ResolveRelativeLevels.apply()

      L1_PrepareDeclarations.apply()

      L1_PrepareAccesses.apply()
      L1_ResolveVariableAccesses.apply()

      L1_ResolveSpecialConstants.apply()
      L1_ResolveMathFunctions.apply()
      //      L1_ResolveEvaluateFunctions.apply()
      //      L1_ResolveIntegrateFunctions.apply()

      var matches = 0
      do {
        matches = 0
        matches += L1_ProcessDeclarations.applyAndCountMatches()
        matches += L1_ResolveAccesses.applyAndCountMatches()

        //        if (Knowledge.experimental_l1_resolveVirtualFields) {
        //          // integrate before evaluate -> might be nested
        //          L1_ResolveIntegrateOnGrid.apply()
        //          matches += (if (L1_ResolveIntegrateOnGrid.results.isEmpty) 0 else L1_ResolveIntegrateOnGrid.results.last._2.matches)
        //
        //          L1_ResolveEvaluateOnGrid.apply()
        //          matches += (if (L1_ResolveEvaluateOnGrid.results.isEmpty) 0 else L1_ResolveEvaluateOnGrid.results.last._2.matches)
        //        }
      } while (matches > 0)

      L1_ProcessBoundaryDeclarations.apply()
    }

    // print before processing
    print()

    // resolve domain aliases
    L1_DomainCollection.handleAliases()

    // process discretization
    L1_ProcessDiscretizationHints.apply()

    // progress knowledge to L2
    L1_KnowledgeContainer.progress()

    ExaRootNode.progressToL2()

    if (Settings.timeStrategies) StrategyTimer.stopTiming("Handling Layer 1")
  }
}
