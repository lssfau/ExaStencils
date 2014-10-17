package exastencils.optimization

import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge.Knowledge
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.util.SimplifyExpression

object Unrolling extends DefaultStrategy("Loop unrolling") {

  final val NO_REM_ANNOT : String = "UnrNR"

  this += new Transformation("optimize", UnrollInnermost)
}

private final case class UnrollException(msg : String) extends Exception(msg)

private final object UnrollInnermost extends PartialFunction[Node, Transformation.OutputType] {

  private final val DEBUG : Boolean = false

  private final val SKIP_ANNOT : String = "UnRSkip"

  def isDefinedAt(node : Node) : Boolean = {
    return node match {
      case loop : ForLoopStatement with OptimizationHint =>
        loop.isInnermost && !loop.removeAnnotation(SKIP_ANNOT).isDefined
      case _ =>
        false
    }
  }

  def apply(node : Node) : Transformation.OutputType = {

    val loop = node.asInstanceOf[ForLoopStatement with OptimizationHint]
    val annot : Option[Annotation] = node.removeAnnotation(Unrolling.NO_REM_ANNOT)
    val oldOffset : Long = if (annot.isDefined) annot.get.value.asInstanceOf[Long] else 0L
    var njuOffset : Long = 0

    var njuBegin : VariableDeclarationStatement = null
    var njuEnd : Expression = null
    var njuIncr : Statement = null
    var itVar : String = null
    var oldInc : Long = 0
    try {
      // do not modify original structure, as an exception could be thrown, which requires a rollback
      val res : (Statement, String, Long) = updateIncrement(loop.inc)
      njuIncr = res._1
      itVar = res._2
      oldInc = res._3
      njuOffset = oldInc * Knowledge.opt_unroll - 1 - oldOffset
      njuEnd = updateEnd(loop.end, itVar, njuOffset)
      njuBegin = updateBegin(loop.begin, itVar)
    } catch {
      case UnrollException(msg) =>
        if (DEBUG)
          Logger.debug("[unroll]  unable to unroll loop: " + msg)
        return node
    }

    val res = new ListBuffer[Statement]()
    if (njuBegin != null) {
      if (loop.isInstanceOf[OMP_PotentiallyParallel]) { // allow omp parallelization
        loop.asInstanceOf[OMP_PotentiallyParallel].addOMPStatements += "lastprivate(" + itVar + ')'
        loop.begin = AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype())), njuBegin.expression.get, "=")
        njuBegin.expression = None
      } else
        loop.begin = NullStatement // only remove if we add a new declaration before
      res += njuBegin
    }
    res += loop // update fields later
    if (oldOffset == 0)
      res += new ForLoopStatement(NullStatement, Duplicate(loop.end),
        Duplicate(loop.inc), Duplicate(loop.body), Duplicate(loop.reduction))
    loop.end = njuEnd
    loop.inc = njuIncr
    loop.body = duplicateStmts(loop.body, itVar, oldInc, loop.isParallel)

    loop.annotate(SKIP_ANNOT)
    loop.annotate(Unrolling.NO_REM_ANNOT, njuOffset)
    if (njuBegin != null)
      return new Scope(res)
    else
      return res
  }

  private def updateIncrement(inc : Statement) : (Statement, String, Long) = {
    return inc match {
      case AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype())),
        AdditionExpression(VariableAccess(itVar2, Some(IntegerDatatype())), IntegerConstant(incr)),
        "=") if (itVar == itVar2) =>
        (AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype())),
          IntegerConstant(incr * Knowledge.opt_unroll), "+="), itVar, incr)

      case AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype())),
        AdditionExpression(IntegerConstant(incr), VariableAccess(itVar2, Some(IntegerDatatype()))),
        "=") if (itVar == itVar2) =>
        (AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype())),
          IntegerConstant(incr * Knowledge.opt_unroll), "+="), itVar, incr)

      case AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype())), IntegerConstant(incr),
        "+=") =>
        (AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype())),
          IntegerConstant(incr * Knowledge.opt_unroll), "+="), itVar, incr)

      case AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype())), IntegerConstant(incr),
        "-=") =>
        (AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype())),
          IntegerConstant(incr * Knowledge.opt_unroll), "-="), itVar, incr)

      case _ =>
        throw new UnrollException("cannot determine stride: " + inc.prettyprint())
    }
  }

  private def updateEnd(end : Expression, itVar : String, offset : Long) : Expression = {
    return end match {
      case LowerEqualExpression(VariableAccess(itVar2, Some(IntegerDatatype())), bound) if (itVar == itVar2) =>
        LowerEqualExpression(VariableAccess(itVar, Some(IntegerDatatype())), SubtractionExpression(bound, IntegerConstant(offset)))

      case LowerExpression(VariableAccess(itVar2, Some(IntegerDatatype())), bound) if (itVar == itVar2) =>
        LowerExpression(VariableAccess(itVar, Some(IntegerDatatype())), SubtractionExpression(bound, IntegerConstant(offset)))

      case GreaterEqualExpression(VariableAccess(itVar2, Some(IntegerDatatype())), bound) if (itVar == itVar2) =>
        GreaterEqualExpression(VariableAccess(itVar, Some(IntegerDatatype())), AdditionExpression(bound, IntegerConstant(offset)))

      case GreaterExpression(VariableAccess(itVar2, Some(IntegerDatatype())), bound) if (itVar == itVar2) =>
        GreaterExpression(VariableAccess(itVar, Some(IntegerDatatype())), AdditionExpression(bound, IntegerConstant(offset)))

      case _ =>
        throw new UnrollException("cannot interpret loop end: " + end.prettyprint())
    }
  }

  private def updateBegin(begin : Statement, itVar : String) : VariableDeclarationStatement = {
    return begin match {
      case NullStatement =>
        null
      case acc @ AssignmentStatement(VariableAccess(itVar2, Some(IntegerDatatype())), init, _) if (itVar == itVar2) =>
        null
      case decl @ VariableDeclarationStatement(IntegerDatatype(), itVar2, Some(init)) if (itVar == itVar2) =>
        decl

      case _ => throw new UnrollException("cannot interpret loop begin: " + begin.prettyprint())
    }
  }

  private def duplicateStmts(body : ListBuffer[Statement], itVar : String, oldInc : Long,
    isParallel : Boolean) : ListBuffer[Statement] = {

    var njuBody : ListBuffer[Statement] = null
    val replaceStrat = new UpdateLoopVarAndNames(itVar)
    val dups = new ListBuffer[Iterator[Statement]]()

    for (i <- 1L until Knowledge.opt_unroll) {
      val dup = Duplicate(body)
      replaceStrat.offset = i * oldInc
      replaceStrat.applyStandalone(Scope(dup))
      dups += dup.iterator.filterNot(s => s.isInstanceOf[CommentStatement])
    }

    if (Knowledge.opt_unroll_interleave && isParallel) {
      njuBody = new ListBuffer[Statement]()
      for (stmt <- body) {
        njuBody += stmt // reuse original statement
        if (!stmt.isInstanceOf[CommentStatement])
          for (iter <- dups)
            njuBody += iter.next()
      }
    } else {
      njuBody = body // reuse original statements
      for (dup <- dups)
        njuBody ++= dup
    }

    return njuBody
  }

  private class UpdateLoopVarAndNames(itVar : String)
      extends DefaultStrategy("Add loop var offset and rename declarations") {

    private final val SKIP_ANNOT = "UpLVSkip"
    private val rename = new HashSet[String]()

    var offset : Long = 0

    this += new Transformation("apply", {
      case vAcc @ VariableAccess(v, Some(IntegerDatatype())) if (v == itVar) =>
        if (!vAcc.removeAnnotation(SKIP_ANNOT).isDefined) {
          vAcc.annotate(SKIP_ANNOT) // already done
          AdditionExpression(vAcc, IntegerConstant(offset))
        } else
          vAcc

      case vAcc : VariableAccess =>
        if (rename.contains(vAcc.name))
          vAcc.name += "_" + offset
        vAcc

      case decl : VariableDeclarationStatement =>
        rename.add(decl.name)
        decl.name += "_" + offset
        decl
    })

    private object IndexCleaner extends Collector {
      override def enter(node : Node) : Unit = {}
      override def leave(node : Node) : Unit = {
        node match {
          case a : ArrayAccess => a.index = SimplifyExpression.simplifyIntegralExpr(a.index)
          case _               =>
        }
      }
      override def reset() : Unit = {}
    }

    override def applyStandalone(node : Node) : Unit = {
      val oldLvl = Logger.getLevel
      Logger.setLevel(1)
      StateManager.register(IndexCleaner) // clean index expressions after modification in replaceStrat
      super.applyStandalone(node)
      StateManager.unregister(IndexCleaner)
      Logger.setLevel(oldLvl)
    }
  }
}
