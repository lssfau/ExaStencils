//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.app.l3

import exastencils.app.LayerHandler
import exastencils.base.ExaRootNode
import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.config._
import exastencils.datastructures.StrategyTimer
import exastencils.domain.l3.L3_DomainCollection
import exastencils.field.l3._
import exastencils.grid.l3._
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.operator.l3._
import exastencils.parsers.l3._
import exastencils.prettyprinting.Indenter
import exastencils.scheduling.Scheduler
import exastencils.solver.l3._
import exastencils.util.l3._

/// L3_LayerHandler

trait L3_LayerHandler extends LayerHandler

/// L3_DummyLayerHandler

object L3_DummyLayerHandler extends L3_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  def initialize() : Unit = {}
  def schedule() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

/// L3_DefaultLayerHandler

object L3_DefaultLayerHandler extends L3_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  override def initialize() : Unit = {
    // activate default knowledge collections

    L3_DomainCollection
    L3_FieldCollection
    L3_StencilCollection
    L3_StencilFieldCollection
    L3_VirtualFieldCollection
    L3_EquationCollection
    L3_FieldCombinationCollection
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

  override def schedule() : Unit = {
    if (Settings.timeStrategies) StrategyTimer.startTiming("Handling Layer 3")

    ExaRootNode.mergeL3(L3_Root(Settings.getL3file.map(L3_Parser.parseFile(_) : L3_Node)))
    ExaRootNode.l3_root.flatten()
    print()

    if (ExaRootNode.l3_root.nodes.nonEmpty) {
      L3_Validation.apply()

      L3_ProcessInlineKnowledge.apply()

      L3_UnifyGlobalSections.apply()

      print()

      // pre-process level specifications in declarations
      L3_ResolveLevelSpecifications.apply()

      L3_ResolveFunctionInstantiations.apply()
      L3_UnfoldFunctionDeclarations.apply()
      L3_ProcessFunctionDeclarations.apply()

      L3_UnfoldKnowledgeDeclarations.apply()
      L3_UnfoldLeveledExpressionDeclarations.apply()
      L3_UnfoldLeveledVariableDeclarations.apply()
      L3_UnfoldSolverModifications.apply()

      // resolve current, etc.
      L3_ResolveRelativeLevels.apply()

      L3_ResolveLevelScopes.apply()

      L3_PrepareSolverForEquations.apply()
      L3_PrepareDeclarations.apply()

      L3_PrepareAccesses.apply()
      L3_InlineDeclaredExpressions.apply()
      L3_ResolveVariableAccesses.apply()

      L3_ResolveSpecialConstants.apply()
      L3_ResolveFrozenFields.apply()
      L3_ResolveDslFunctionReferences.apply()
      L3_ResolveMathFunctions.apply()
      L3_ResolveEvaluateFunctions.apply()
      L3_ResolveIntegrateFunctions.apply()

      var matches = 0
      do {
        matches = 0
        matches += L3_ProcessDeclarations.applyAndCountMatches()

        L3_ProcessSolverForEquations.apply()
        matches += (if (L3_ProcessSolverForEquations.results.isEmpty) 0 else L3_ProcessSolverForEquations.results.last._2.matches)

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

      L3_FieldCollection.addInitFieldsFunction()

      L3_ResolveSolverForEquations.apply()

      L3_ResolveFieldFieldConvolutions.apply()
      L3_ResolveOperatorTimesField.apply()

      L3_IntroduceSlots.apply()
    }

    // process application Hints
    L3_ProcessApplicationHints.apply()

    // print before processing
    print()

    // progress knowledge to L4
    L3_KnowledgeContainer.progress()

    ExaRootNode.progressToL4()

    if (Settings.timeStrategies) StrategyTimer.stopTiming("Handling Layer 3")
  }
}
