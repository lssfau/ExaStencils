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
import exastencils.parsers.l1._
import exastencils.prettyprinting.Indenter
import exastencils.scheduling._
import exastencils.scheduling.l1._
import exastencils.solver.l1._
import exastencils.util.l1._

/// L1_LayerHandler

trait L1_LayerHandler extends LayerHandler

/// L1_DummyLayerHandler

object L1_DummyLayerHandler extends L1_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  def initialize() : Unit = {}
  def schedule() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

/// L1_DefaultLayerHandler

object L1_DefaultLayerHandler extends L1_LayerHandler {
  var scheduler : Scheduler = Scheduler()

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

  override def schedule() : Unit = {
    scheduler.register(StrategyTimerWrapper(start = true, "Handling Layer 1"))

    // add L1 code here
    scheduler.register(MergeExaRootNodeWrapper(L1_Root(Settings.getL1file.map(L1_Parser.parseFile(_) : L1_Node))))
    scheduler.register(PrintLayerWrapper(this))

    scheduler.register(ConditionedStrategyContainerWrapper(() => ExaRootNode.l1_root.nodes.nonEmpty,
      L1_Validation,

      L1_ProcessInlineKnowledge,

      L1_UnifyGlobalSections,

      // pre-process level specifications in declarations
      L1_ResolveLevelSpecifications,

      L1_UnfoldKnowledgeDeclarations,
      L1_UnfoldLeveledVariableDeclarations,

      // resolve current, etc.
      L1_ResolveRelativeLevels,

      L1_PrepareDeclarations,

      L1_PrepareAccesses,
      L1_ResolveVariableAccesses,

      L1_ResolveSpecialConstants,
      L1_ResolveMathFunctions,

      L1_ProcessDeclarationsAndResolveAccessesWrapper,

      L1_ProcessBoundaryDeclarations))

    // print before processing
    scheduler.register(PrintLayerWrapper(this))

    // resolve domain aliases
    scheduler.register(L1_HandleDomainAliasesWrapper)

    // process discretization
    scheduler.register(L1_ProcessDiscretizationHints)

    // progress knowledge to L2
    scheduler.register(ProgressKnowledgeContainerWrapper(this))

    scheduler.register(ProgressExaRootNodeWrapper(this))

    scheduler.register(StrategyTimerWrapper(start = false, "Handling Layer 1"))
  }
}
