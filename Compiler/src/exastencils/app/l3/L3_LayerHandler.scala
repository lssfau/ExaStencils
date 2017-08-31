package exastencils.app.l3

import exastencils.app.LayerHandler
import exastencils.base.ExaRootNode
import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.config._
import exastencils.datastructures.StrategyTimer
import exastencils.deprecated.l3Generate
import exastencils.domain.l3.L3_DomainCollection
import exastencils.field.l3._
import exastencils.grid.l3._
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.logger.Logger
import exastencils.operator.l3._
import exastencils.parsers.l3.L3_Parser
import exastencils.prettyprinting.Indenter
import exastencils.solver.l3._
import exastencils.util.l3.L3_ResolveMathFunctions

/// L3_LayerHandler

trait L3_LayerHandler extends LayerHandler

/// L3_DummyLayerHandler 

object L3_DummyLayerHandler extends L3_LayerHandler {
  def initialize() : Unit = {}
  def handle() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

/// L3_DefaultLayerHandler

object L3_DefaultLayerHandler extends L3_LayerHandler {
  override def initialize() : Unit = {
    // activate default knowledge collections

    L3_DomainCollection
    L3_FieldCollection
    L3_StencilCollection
    L3_StencilFieldCollection
    L3_VirtualFieldCollection
    L3_EquationCollection
  }

  override def shutdown() : Unit = {
    L3_KnowledgeContainer.clear()
  }

  override def print() : Unit = {
    if (Settings.getDebugL3file.nonEmpty) {
      val outFile = new java.io.FileWriter(Settings.getDebugL3file)
      outFile.write(Indenter.addIndentations(ExaRootNode.l3_root.prettyprint()))
      outFile.close()
    }
  }

  override def handle() : Unit = {
    if (Settings.timeStrategies) StrategyTimer.startTiming("Handling Layer 3")

    if (Knowledge.experimental_layerExtension) {
      ExaRootNode.mergeL3(L3_Root(Settings.getL3file.map(L3_Parser.parseFile(_) : L3_Node)))

      L3_UnifyGlobalSections.apply()

      print()

      // pre-process level specifications in declarations
      L3_ResolveLevelSpecifications.apply()

      L3_ResolveFunctionInstantiations.apply()
      L3_UnfoldFunctionDeclarations.apply()
      L3_ProcessFunctionDeclarations.apply()

      L3_UnfoldKnowledgeDeclarations.apply()
      L3_UnfoldLeveledVariableDeclarations.apply()

      // resolve current, etc.
      L3_ResolveRelativeLevels.apply()

      L3_PrepareDeclarations.apply()

      L3_PrepareAccesses.apply()
      L3_ResolveVariableAccesses.apply()

      L3_ResolveLevelScopes.apply()

      L3_ResolveDslFunctionReferences.apply()
      L3_ResolveMathFunctions.apply()
      L3_ResolveEvaluateFunctions.apply()
      L3_ResolveIntegrateFunctions.apply()

      var matches = 0
      do {
        matches = 0
        matches += L3_ProcessDeclarations.applyAndCountMatches()
        matches += L3_ResolveAccesses.applyAndCountMatches()

        if (Knowledge.experimental_l3_resolveVirtualFields) {
          // integrate before evaluate -> might be nested
          L3_ResolveIntegrateOnGrid.apply()
          matches += (if (L3_ResolveIntegrateOnGrid.results.isEmpty) 0 else L3_ResolveIntegrateOnGrid.results.last._2.matches)

          L3_ResolveEvaluateOnGrid.apply()
          matches += (if (L3_ResolveEvaluateOnGrid.results.isEmpty) 0 else L3_ResolveEvaluateOnGrid.results.last._2.matches)
        }
      } while (matches > 0)

      L3_ProcessBoundaryDeclarations.apply()
      L3_ProcessFieldOverrides.apply()

      L3_ResolveSolverForEquations.apply()

      L3_ResolveFieldFieldConvolutions.apply()
      L3_ResolveOperatorTimesField.apply()

      L3_FieldCollection.addInitFieldsFunction()

      // progress knowledge to L4
      L3_KnowledgeContainer.progress()

      ExaRootNode.progressToL4()
    } else if (Knowledge.l3tmp_generateL4) {
      val l3gen_root = l3Generate.Root()
      val l4Filenames = Settings.getL4file
      if (l4Filenames.length != 1) Logger.error("l3tmp_generateL4 requires exactly one Layer4 file provided in settings")
      l3gen_root.printToL4(l4Filenames.head)
    }

    if (Settings.timeStrategies) StrategyTimer.stopTiming("Handling Layer 3")
  }
}
