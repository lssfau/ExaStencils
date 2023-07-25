package exastencils.scheduling.l4

import exastencils.base.ExaRootNode
import exastencils.boundary.ir.L4_ResolveBoundaryHandlingFunctions
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.datastructures.Node
import exastencils.field.l4._
import exastencils.grid.l4._
import exastencils.knowledge.l4._
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.logger.Logger
import exastencils.optimization.l4.L4_GeneralSimplify
import exastencils.parsers.l4.L4_Parser
import exastencils.scheduling.NoStrategyWrapper
import exastencils.scheduling.Schedulable

/// L4_SecondParseWrapper

object L4_SecondParseWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    val oldL4Code = ExaRootNode.l4_root.prettyprint()

    // re-parse the code to check for errors - also clear knowledge collections
    L4_KnowledgeContainer.clear()

    val l4FileName = if (Settings.getDebugL4file.nonEmpty) Settings.getDebugL4file else "debugLayer4"
    try {
      ExaRootNode.l4_root = L4_Parser.parse(oldL4Code, l4FileName)
    } catch {
      case exc : Exception => Logger.error("Second parse error: " + exc.getMessage)
    }
    ExaRootNode.l4_root.flatten()
  }
}

/// L4_ProcessDeclarationsAndResolveAccessesWrapper

object L4_ProcessDeclarationsAndResolveAccessesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
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
  }
}

/// L4_ProcessKnowledgeDeclarationsWrapper

object L4_ProcessKnowledgeDeclarationsWrapper extends Schedulable {
  override def apply(applyAtNode : Option[Node]) : Unit = L4_ProcessKnowledgeDeclarations.apply()
}

/// L4_ResolveBoundaryHandlingFunctionsWrapper

object L4_ResolveBoundaryHandlingFunctionsWrapper extends Schedulable {
  override def apply(applyAtNode : Option[Node]) : Unit = L4_ResolveBoundaryHandlingFunctions.apply()
}

/// L4_GeneralSimplifyUntilDoneWrapper

object L4_GeneralSimplifyUntilDoneWrapper extends Schedulable {
  override def apply(applyAtNode : Option[Node]) : Unit = {
    L4_GeneralSimplify.doUntilDone(applyAtNode)
  }
}

/// L4_DuplicateFieldLayoutsForFieldsWrapper

object L4_DuplicateFieldLayoutsForFieldsWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Knowledge.l4_genSepLayoutsPerField)
      L4_DuplicateFieldLayoutsForFields.apply()
  }
}
