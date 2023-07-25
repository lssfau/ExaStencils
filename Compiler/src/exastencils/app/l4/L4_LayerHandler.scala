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

package exastencils.app.l4

import exastencils.app.LayerHandler
import exastencils.applications.l4.L4_AddDefaultApplication
import exastencils.base.ExaRootNode
import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.communication.l4._
import exastencils.config._
import exastencils.domain.l4.L4_DomainCollection
import exastencils.field.l4._
import exastencils.grid.l4._
import exastencils.interfacing.l4.L4_ExternalFieldCollection
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.layoutTransformation.l4.L4_AddSoAtoAoSTransformation
import exastencils.operator.l4._
import exastencils.parsers.l4._
import exastencils.prettyprinting.Indenter
import exastencils.scheduling._
import exastencils.scheduling.ir.IR_AddPaddingToFieldLayoutsWrapper
import exastencils.scheduling.l4._
import exastencils.solver.l4._
import exastencils.timing.l4.L4_ResolveTimerFunctions
import exastencils.util.l4._

/// L4_LayerHandler

trait L4_LayerHandler extends LayerHandler

/// L4_DummyLayerHandler

object L4_DummyLayerHandler extends L4_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  def initialize() : Unit = {}
  def schedule() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

/// L4_DefaultLayerHandler

object L4_DefaultLayerHandler extends L4_LayerHandler {
  var scheduler : Scheduler = Scheduler()

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


    L4_PrepareAccesses.strategies += L4_PrepareMatrixAccesses
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

  override def schedule() : Unit = {
    scheduler.register(StrategyTimerWrapper(start = true, "Handling Layer 4"))

    scheduler.register(ConditionedStrategyWrapper(() => ExaRootNode.l4_root.nodes.nonEmpty,
      L4_WrapFieldFieldConvolutions,
      L4_AddLoopsToFieldAssignments,
      L4_AddLoopsToLocalSolve,
      L4_AddCommunicationToLoops,
      L4_AdaptFieldLayoutsForComm))

    scheduler.register(MergeExaRootNodeWrapper(L4_Root(Settings.getL4file.map(L4_Parser.parseFile(_) : L4_Node))))

    scheduler.register(ConditionedStrategyWrapper(true,
      L4_UnresolveOperatorTimesField,
      L4_UnresolveFieldFieldConvolutions,
      L4_UnresolveStencilAccesses,
      L4_UnresolveStencilFieldAccesses,
      L4_UnresolveFieldAccesses,
      // FIXME: transform back to declarations and re-fold
      L4_ReplaceLevelsInFunctionDecls,
      L4_CombineLeveledFunctionDecls,
    ))

    scheduler.register(ConditionedStrategyWrapper(Knowledge.l4_genSoA2AoSTransformation, L4_AddSoAtoAoSTransformation))

    scheduler.register(PrintLayerWrapper(this))

    scheduler.register(L4_SecondParseWrapper)

    scheduler.register(ConditionedStrategyWrapper(() => ExaRootNode.l4_root.nodes.nonEmpty,
      L4_ProcessInlineKnowledge,

      ConditionedStrategyWrapper(Knowledge.l4_genDefaultApplication, L4_AddDefaultApplication),

      L4_Validation,

      StrategyTimerWrapper(start = false, "Handling Layer 4"),

      L4_UnifyGlobalSections,

      // go to IR

      L4_ResolveColorLoops,

      // pre-process level specifications in declarations
      L4_ResolveLevelSpecifications,

      L4_ResolveFunctionInstantiations,
      L4_UnfoldFunctionDeclarations,
      L4_ProcessFunctionDeclarations,

      L4_UnfoldKnowledgeDeclarations,
      L4_UnfoldLeveledExpressionDeclarations,
      L4_UnfoldLeveledVariableDeclarations,

      // resolve current, etc.
      L4_ResolveRelativeLevels,

      L4_ResolveLevelScopes,

      L4_PrepareDeclarations,

      L4_InlineDeclaredExpressions,
      L4_PrepareAccesses,
      L4_ResolveLoopVariables,
      L4_ResolveVariableAccesses,

      L4_ResolveSpecialConstants,
      L4_ResolveFrozenFields,
      L4_ResolveDslFunctionReferences,
      L4_ResolveEvaluateFunctions,
      L4_ResolveIntegrateFunctions,
      L4_ResolveMathFunctions,
      L4_ResolveTimerFunctions,

      L4_ProcessDeclarationsAndResolveAccessesWrapper,

      L4_ResolveVariableAccesses,
      L4_ResolveStencilFunctions,
      L4_ResolveLoopItAccesses,
      L4_ResolveNativeFunctions,
      L4_ResolvePrintFunctions,
      L4_ResolveBuildStringFunctions,
      L4_ResolveKnowledgeParameterAccess,

      L4_GeneralSimplifyUntilDoneWrapper,

      L4_ProcessKnowledgeDeclarationsWrapper,

      L4_DuplicateFieldLayoutsForFieldsWrapper,

      // after L4_ResolveFieldAccesses
      L4_ResolveReadFieldFunctions,
      L4_ResolvePrintFieldFunctions,
      L4_ResolveWriteFieldFunctions,

      L4_ResolveBoundaryHandlingFunctionsWrapper,

      L4_ResolveStencilComponentAccesses,
      L4_ResolveStencilFieldComponentAccesses,

      L4_ValidateComplexAccess))

    scheduler.register(StrategyTimerWrapper(start = true, "Progressing from L4 to IR"))

    // progress knowledge to IR
    scheduler.register(ProgressKnowledgeContainerWrapper(this))

    scheduler.register(IR_AddPaddingToFieldLayoutsWrapper)

    scheduler.register(ProgressExaRootNodeWrapper(this))

    scheduler.register(StrategyTimerWrapper(start = false, "Progressing from L4 to IR"))
  }
}
