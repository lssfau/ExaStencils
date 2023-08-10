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
import exastencils.domain.l3.L3_DomainCollection
import exastencils.field.l3._
import exastencils.fieldlike.l3.L3_FieldLikeCollections
import exastencils.fieldlike.l3.L3_ProcessBoundaryDeclarations
import exastencils.grid.l3._
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.operator.l3._
import exastencils.parsers.l3._
import exastencils.prettyprinting.Indenter
import exastencils.scheduling._
import exastencils.scheduling.l3._
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
    L3_FieldLikeCollections.clear()
  }

  override def print() : Unit = {
    if (Settings.getDebugL3file.nonEmpty) {
      val outFile = new java.io.FileWriter(Settings.getDebugL3file)
      outFile.write(Indenter.addIndentations(ExaRootNode.l3_root.prettyprint()))
      outFile.close()
    }
  }

  override def schedule() : Unit = {
    scheduler.register(StrategyTimerWrapper(start = true, "Handling Layer 3"))

    scheduler.register(MergeExaRootNodeWrapper(L3_Root(Settings.getL3file.map(L3_Parser.parseFile(_) : L3_Node))))

    scheduler.register(PrintLayerWrapper(this))

    scheduler.register(ConditionedStrategyContainerWrapper(() => ExaRootNode.l3_root.nodes.nonEmpty,
      L3_Validation,

      L3_ProcessInlineKnowledge,

      L3_UnifyGlobalSections,

      PrintLayerWrapper(this),

      // pre-process level specifications in declarations
      L3_ResolveLevelSpecifications,

      L3_ResolveFunctionInstantiations,
      L3_UnfoldFunctionDeclarations,
      L3_ProcessFunctionDeclarations,

      L3_UnfoldKnowledgeDeclarations,
      L3_UnfoldLeveledExpressionDeclarations,
      L3_UnfoldLeveledVariableDeclarations,
      L3_UnfoldSolverModifications,

      // resolve current, etc.
      L3_ResolveRelativeLevels,

      L3_ResolveLevelScopes,

      L3_PrepareSolverForEquations,
      L3_PrepareDeclarations,

      L3_PrepareAccesses,
      L3_InlineDeclaredExpressions,
      L3_ResolveVariableAccesses,

      L3_ResolveSpecialConstants,
      L3_ResolveFrozenFields,
      L3_ResolveDslFunctionReferences,
      L3_ResolveMathFunctions,
      L3_ResolveEvaluateFunctions,
      L3_ResolveIntegrateFunctions,

      L3_ProcessDeclarationsAndResolveAccessesWrapper,

      L3_ProcessBoundaryDeclarations,
      L3_ProcessFieldOverrides,

      L3_AddInitFieldsFunctionWrapper,

      L3_ResolveSolverForEquations,

      L3_ResolveFieldFieldConvolutions,
      L3_ResolveOperatorTimesField,

      L3_IntroduceSlots))

    // process application Hints
    scheduler.register(L3_ProcessApplicationHints)

    // print before processing
    scheduler.register(PrintLayerWrapper(this))

    // progress knowledge to L4
    scheduler.register(ProgressKnowledgeContainerWrapper(this))

    scheduler.register(ProgressExaRootNodeWrapper(this))

    scheduler.register(StrategyTimerWrapper(start = false, "Handling Layer 3"))
  }
}
