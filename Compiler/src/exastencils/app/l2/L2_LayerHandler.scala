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
import exastencils.domain.l2._
import exastencils.field.l2._
import exastencils.grid.l2._
import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.knowledge.l2._
import exastencils.operator.l2._
import exastencils.parsers.l2._
import exastencils.prettyprinting.Indenter
import exastencils.scheduling._
import exastencils.scheduling.l2._
import exastencils.solver.l2._
import exastencils.util.l2._

/// L2_LayerHandler

trait L2_LayerHandler extends LayerHandler

/// L2_DummyLayerHandler

object L2_DummyLayerHandler extends L2_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  def initialize() : Unit = {}
  def schedule() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

/// L2_DefaultLayerHandler

object L2_DefaultLayerHandler extends L2_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  override def initialize() : Unit = {
    // activate default knowledge collections

    L2_DomainCollection
    L2_FieldCollection
    L2_StencilCollection
    L2_StencilFieldCollection
    L2_VirtualFieldCollection
    L2_EquationCollection
    L2_FieldCombinationCollection
  }

  override def shutdown() : Unit = {
    L2_KnowledgeContainer.clear()
  }

  override def print() : Unit = {
    if (Settings.getDebugL2file.nonEmpty) {
      val outFile = new java.io.FileWriter(Settings.getDebugL2file)
      outFile.write(Indenter.addIndentations(ExaRootNode.l2_root.prettyprint()))
      outFile.close()
    }
  }

  override def schedule() : Unit = {
    scheduler.register(StrategyTimerWrapper(start = true, "Handling Layer 2"))

    scheduler.register(MergeExaRootNodeWrapper(L2_Root(Settings.getL2file.map(L2_Parser.parseFile(_) : L2_Node))))
    scheduler.register(PrintLayerWrapper(this))

    scheduler.register(ConditionedStrategyWrapper(() => ExaRootNode.l2_root.nodes.nonEmpty,
      L2_Validation,

      L2_ProcessInlineKnowledge,

      L2_UnifyGlobalSections,

      // pre-process level specifications in declarations
      L2_ResolveLevelSpecifications,

      L2_UnfoldKnowledgeDeclarations,
      L2_UnfoldLeveledExpressionDeclarations,
      L2_UnfoldLeveledVariableDeclarations,

      // resolve current, etc.
      L2_ResolveRelativeLevels,

      L2_PrepareDeclarations,

      L2_PrepareAccesses,
      L2_InlineDeclaredExpressions,
      L2_ResolveVariableAccesses,

      L2_ResolveSpecialConstants,
      L2_ResolveFrozenFields,
      L2_ResolveMathFunctions,
      L2_ResolveEvaluateFunctions,
      L2_ResolveIntegrateFunctions,

      L2_ProcessDeclarationsAndResolveAccessesWrapper,

      L2_ProcessBoundaryDeclarations))

    // print before processing
    scheduler.register(PrintLayerWrapper(this))

    // process solver
    scheduler.register(L2_ProcessSolverHints)

    // progress knowledge to L3
    scheduler.register(ProgressKnowledgeContainerWrapper(this))

    scheduler.register(ProgressExaRootNodeWrapper(this))

    scheduler.register(StrategyTimerWrapper(start = false, "Handling Layer 2"))
  }
}
