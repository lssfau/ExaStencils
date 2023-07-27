package exastencils.scheduling

import scala.collection.mutable.ListBuffer

import exastencils.app.LayerHandler
import exastencils.app.l1.L1_LayerHandler
import exastencils.app.l2.L2_LayerHandler
import exastencils.app.l3.L3_LayerHandler
import exastencils.app.l4.L4_LayerHandler
import exastencils.base.ExaRootNode
import exastencils.base.ir.IR_Root
import exastencils.base.l1.L1_Root
import exastencils.base.l2.L2_Root
import exastencils.base.l3.L3_Root
import exastencils.base.l4.L4_Root
import exastencils.config.Settings
import exastencils.datastructures.Node
import exastencils.datastructures.StrategyTimer
import exastencils.knowledge.l1.L1_KnowledgeContainer
import exastencils.knowledge.l2.L2_KnowledgeContainer
import exastencils.knowledge.l3.L3_KnowledgeContainer
import exastencils.knowledge.l4.L4_KnowledgeContainer
import exastencils.logger.Logger

/// Schedulable

trait Schedulable {
  def apply(applyAtNode : Option[Node] = None) : Unit
  def reset() : Unit = {}
}

/// SingleSchedulable

trait SingleSchedulable extends Schedulable

/// SchedulableContainer

trait SchedulableContainer extends Schedulable {
  def strats : ListBuffer[SingleSchedulable]
}

/// NoStrategyWrapper

trait NoStrategyWrapper extends SingleSchedulable {
  def callback : () => Unit

  override def apply(applyAtNode : Option[Node]) : Unit = callback()
}

/// ConditionedStrategyWrapper

object ConditionedStrategyContainerWrapper {
  def apply(condition : Boolean, strats : Schedulable*) = new ConditionedStrategyContainerWrapper(() => condition,
    strats.flatMap {
      case s : SingleSchedulable                     => List(s)
      case css : ConditionedSingleStrategyWrapper    => List(ConditionedSingleStrategyWrapper(() => condition && css.callbackCondition(), css))
      case csc : ConditionedStrategyContainerWrapper => csc.strats.map(s => ConditionedSingleStrategyWrapper(() => condition && csc.callbackCondition(), s))
      case c : SchedulableContainer                  => c.strats.map(s => ConditionedSingleStrategyWrapper(() => condition, s))
    }.to[ListBuffer])

  def apply(callbackCondition : () => Boolean, strats : Schedulable*) = new ConditionedStrategyContainerWrapper(callbackCondition,
    strats.flatMap {
      case s : SingleSchedulable                     => List(s)
      case css : ConditionedSingleStrategyWrapper    => List(ConditionedSingleStrategyWrapper(() => callbackCondition() && css.callbackCondition(), css))
      case csc : ConditionedStrategyContainerWrapper => csc.strats.map(s => ConditionedSingleStrategyWrapper(() => callbackCondition() && csc.callbackCondition(), s))
      case c : SchedulableContainer                  => c.strats.map(s => ConditionedSingleStrategyWrapper(() => callbackCondition(), s))
    }.to[ListBuffer])
}

case class ConditionedStrategyContainerWrapper(var callbackCondition : () => Boolean, var strats : ListBuffer[SingleSchedulable]) extends SchedulableContainer {
  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    if (callbackCondition())
      strats.foreach(_.apply())
  }

  override def reset() : Unit = {
    for (strat <- strats)
      strat.reset()
  }
}

/// ConditionedSingleStrategyWrapper

object ConditionedSingleStrategyWrapper {
  def apply(condition : Boolean, strat : SingleSchedulable) = new ConditionedSingleStrategyWrapper(() => condition, strat)

  def apply(callbackCondition : () => Boolean, strat : SingleSchedulable) = new ConditionedSingleStrategyWrapper(callbackCondition, strat)
}

case class ConditionedSingleStrategyWrapper(var callbackCondition : () => Boolean, var strat : SingleSchedulable) extends SingleSchedulable {
  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    if (callbackCondition())
      strat.apply()
  }

  override def reset() : Unit = {
    strat.reset()
  }
}

/// StrategyTimerWrapper

case class StrategyTimerWrapper(var start : Boolean, var name : String) extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Settings.timeStrategies) {
      if (start)
        StrategyTimer.startTiming(name)
      else
        StrategyTimer.stopTiming(name)
    }
  }
}

/// PrintLayerWrapper

case class PrintLayerWrapper(var layerHandler : LayerHandler) extends NoStrategyWrapper {
  override def callback : () => Unit = () => layerHandler.print()
}

/// MergeExaRootNodeWrapper

case class MergeExaRootNodeWrapper(var newRoot : Node) extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    newRoot match {
      case l1 : L1_Root => ExaRootNode.mergeL1(l1); ExaRootNode.l1_root.flatten()
      case l2 : L2_Root => ExaRootNode.mergeL2(l2); ExaRootNode.l2_root.flatten()
      case l3 : L3_Root => ExaRootNode.mergeL3(l3); ExaRootNode.l3_root.flatten()
      case l4 : L4_Root => ExaRootNode.mergeL4(l4); ExaRootNode.l4_root.flatten()
      case ir : IR_Root => ExaRootNode.mergeIR(ir); ExaRootNode.ir_root.flatten()
      case _            => Logger.error("Invalid root node specified for AST merging")
    }
  }
}

/// ProgressExaRootNodeWrapper

case class ProgressExaRootNodeWrapper(var layerHandler : LayerHandler) extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    layerHandler match {
      case _ : L1_LayerHandler => ExaRootNode.progressToL2()
      case _ : L2_LayerHandler => ExaRootNode.progressToL3()
      case _ : L3_LayerHandler => ExaRootNode.progressToL4()
      case _ : L4_LayerHandler => ExaRootNode.progressToIR()
      case _                   => Logger.error("Invalid layer handler specified for progressing AST")
    }
  }
}

/// ProgressExaRootNodeWrapper

case class ProgressKnowledgeContainerWrapper(var layerHandler : LayerHandler) extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    layerHandler match {
      case _ : L1_LayerHandler => L1_KnowledgeContainer.progress()
      case _ : L2_LayerHandler => L2_KnowledgeContainer.progress()
      case _ : L3_LayerHandler => L3_KnowledgeContainer.progress()
      case _ : L4_LayerHandler => L4_KnowledgeContainer.progress()
      case _                   => Logger.error("Invalid layer handler specified for progressing AST")
    }
  }
}
