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

package exastencils.app.l2

import exastencils.app.LayerHandler
import exastencils.base.ExaRootNode
import exastencils.base.l2._
import exastencils.baseExt.l2._
import exastencils.config._
import exastencils.datastructures.StrategyTimer
import exastencils.domain.l2._
import exastencils.field.l2._
import exastencils.fieldlike.l2.L2_FieldLikeCollections
import exastencils.fieldlike.l2.L2_ProcessBoundaryDeclarations
import exastencils.grid.l2._
import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.knowledge.l2._
import exastencils.operator.l2._
import exastencils.parsers.l2._
import exastencils.prettyprinting.Indenter
import exastencils.solver.l2._
import exastencils.util.l2._
import exastencils.waLBerla.l2.field.L2_WaLBerlaFieldCollection

/// L2_LayerHandler

trait L2_LayerHandler extends LayerHandler

/// L2_DummyLayerHandler

object L2_DummyLayerHandler extends L2_LayerHandler {
  def initialize() : Unit = {}
  def handle() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

/// L2_DefaultLayerHandler

object L2_DefaultLayerHandler extends L2_LayerHandler {
  override def initialize() : Unit = {
    // activate default knowledge collections

    L2_DomainCollection
    L2_WaLBerlaFieldCollection
    L2_FieldCollection
    L2_StencilCollection
    L2_StencilFieldCollection
    L2_VirtualFieldCollection
    L2_EquationCollection
    L2_FieldCombinationCollection
  }

  override def shutdown() : Unit = {
    L2_KnowledgeContainer.clear()
    L2_FieldLikeCollections.clear()
  }

  override def print() : Unit = {
    if (Settings.getDebugL2file.nonEmpty) {
      val outFile = new java.io.FileWriter(Settings.getDebugL2file)
      outFile.write(Indenter.addIndentations(ExaRootNode.l2_root.prettyprint()))
      outFile.close()
    }
  }

  override def handle() : Unit = {
    if (Settings.timeStrategies) StrategyTimer.startTiming("Handling Layer 2")

    ExaRootNode.mergeL2(L2_Root(Settings.getL2file.map(L2_Parser.parseFile(_) : L2_Node)))
    ExaRootNode.l2_root.flatten()
    print()

    if (ExaRootNode.l2_root.nodes.nonEmpty) {
      L2_Validation.apply()

      L2_ProcessInlineKnowledge.apply()

      L2_UnifyGlobalSections.apply()

      // pre-process level specifications in declarations
      L2_ResolveLevelSpecifications.apply()

      L2_UnfoldKnowledgeDeclarations.apply()
      L2_UnfoldLeveledExpressionDeclarations.apply()
      L2_UnfoldLeveledVariableDeclarations.apply()

      // resolve current, etc.
      L2_ResolveRelativeLevels.apply()

      L2_PrepareDeclarations.apply()

      L2_PrepareAccesses.apply()
      L2_InlineDeclaredExpressions.apply()
      L2_ResolveVariableAccesses.apply()

      L2_ResolveSpecialConstants.apply()
      L2_ResolveFrozenFields.apply()
      L2_ResolveMathFunctions.apply()
      L2_ResolveEvaluateFunctions.apply()
      L2_ResolveIntegrateFunctions.apply()

      var matches = 0
      do {
        matches = 0
        matches += L2_ProcessDeclarations.applyAndCountMatches()
        matches += L2_ResolveAccesses.applyAndCountMatches()

        if (Knowledge.experimental_l2_resolveVirtualFields) {
          // integrate before evaluate -> might be nested
          L2_ResolveIntegrateOnGrid.apply()
          matches += (if (L2_ResolveIntegrateOnGrid.results.isEmpty) 0 else L2_ResolveIntegrateOnGrid.results.last._2.matches)

          L2_ResolveEvaluateOnGrid.apply()
          matches += (if (L2_ResolveEvaluateOnGrid.results.isEmpty) 0 else L2_ResolveEvaluateOnGrid.results.last._2.matches)
        }
      } while (matches > 0)

      L2_ProcessBoundaryDeclarations.apply()
    }

    // print before processing
    print()

    // process solver
    L2_ProcessSolverHints.apply()

    // progress knowledge to L3
    L2_KnowledgeContainer.progress()

    ExaRootNode.progressToL3()

    if (Settings.timeStrategies) StrategyTimer.stopTiming("Handling Layer 2")
  }
}
