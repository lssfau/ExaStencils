package exastencils.optimization

import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.omp._
import exastencils.util._

object Unrolling extends DefaultStrategy("Loop unrolling") {

  final val UNROLLED_ANNOT : String = "UNDone"

  private[optimization] final val startVar : String = "_start"
  private[optimization] final val intermVar : String = "_intermediate"
  private[optimization] final val endVar : String = "_end"

  private[optimization] def startVarAcc = new VariableAccess(startVar, IntegerDatatype)
  private[optimization] def intermVarAcc = new VariableAccess(intermVar, IntegerDatatype)
  private[optimization] def endVarAcc = new VariableAccess(endVar, IntegerDatatype)

  private[optimization] def getBoundsDeclAndPostLoop(itVar : String, start : Expression, endExcl : Expression, oldIncr : Long,
    body : ListBuffer[Statement], reduction : Option[Reduction]) : (ListBuffer[Statement], Statement) = {

    def itVarAcc = new VariableAccess(itVar, IntegerDatatype)

    val boundsDecls = new ListBuffer[Statement]()
    boundsDecls += new VariableDeclarationStatement(IntegerDatatype, startVar, start)
    boundsDecls += new VariableDeclarationStatement(IntegerDatatype, endVar, endExcl)

    val postBegin = new VariableDeclarationStatement(IntegerDatatype, itVar, intermVarAcc)
    val postEnd = new LowerExpression(itVarAcc, endVarAcc)
    val postIncr = new AssignmentStatement(itVarAcc, IntegerConstant(oldIncr), "+=")

    val postLoop = new ForLoopStatement(postBegin, postEnd, postIncr, body, reduction)

    return (boundsDecls, postLoop)
  }

  private[optimization] def getIntermDecl(newIncr : Long) : VariableDeclarationStatement = {
    return new VariableDeclarationStatement(IntegerDatatype, intermVar, getIntermExpr(newIncr))
  }

  def getIntermExpr(newIncr : Long) : Expression = {
    return new MaximumExpression(startVarAcc, endVarAcc - ((endVarAcc - startVarAcc) Mod IntegerConstant(newIncr)))
  }

  private[optimization] def addBounds(itVar : String, begin : Statement, end : Expression, incr : Expression,
    writeDecls : Boolean, stmts : ListBuffer[Statement]) : Boolean = {

    val (lower, isDecl) : (Expression, Boolean) =
      begin match {
        case AssignmentStatement(VariableAccess(itVar2, Some(IntegerDatatype)), init, "=") if (itVar == itVar2) => (init, false)
        case VariableDeclarationStatement(IntegerDatatype, itVar2, Some(init)) if (itVar == itVar2) => (init, true)

        case _ => throw new UnrollException("cannot interpret loop begin: " + begin.prettyprint())
      }

    val upperExcl : Expression =
      if (writeDecls) end match {
        case LowerExpression(VariableAccess(itVar2, Some(IntegerDatatype)), bound) if (itVar == itVar2) =>
          bound

        case LowerEqualExpression(VariableAccess(itVar2, Some(IntegerDatatype)), bound) if (itVar == itVar2) =>
          new AdditionExpression(bound, IntegerConstant(1))

        case _ => throw new UnrollException("cannot interpret loop end: " + end.prettyprint())
      }
      else null

    addBounds(lower, upperExcl, incr, writeDecls, stmts)

    return isDecl
  }

  private[optimization] def addBounds(lower : Expression, upperExcl : Expression, incr : Expression,
    writeDecls : Boolean, stmts : ListBuffer[Statement]) : Unit = {

    val intermExpr = new MaximumExpression(endVarAcc - ((endVarAcc - startVarAcc) Mod incr), startVarAcc)

    if (writeDecls) {
      stmts += new VariableDeclarationStatement(IntegerDatatype, startVar, lower)
      stmts += new VariableDeclarationStatement(IntegerDatatype, endVar, upperExcl)
      stmts += new VariableDeclarationStatement(IntegerDatatype, intermVar, intermExpr)
    } else
      stmts += new AssignmentStatement(intermVarAcc, intermExpr, "=")
  }

  this += new Transformation("optimize", UnrollInnermost)
}

private final case class UnrollException(msg : String) extends Exception(msg)

private final object UnrollInnermost extends PartialFunction[Node, Transformation.OutputType] {

  private final val DEBUG : Boolean = false
  private final val SKIP_ANNOT : String = "URSkip"

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

    var itVar : String = null
    def itVarAcc = new VariableAccess(itVar, IntegerDatatype)
    var start : Expression = null
    var endExcl : Expression = null
    var oldStride : Long = 0
    var newStride : Long = 0
    try {
      val (itVar_, start_, endExcl_, oldStride_) = extractBoundsAndIncrement(loop.begin, loop.end, loop.inc)
      itVar = itVar_
      start = start_
      endExcl = endExcl_
      oldStride = oldStride_
      newStride = oldStride * Knowledge.opt_unroll
    } catch {
      case UnrollException(msg) =>
        if (DEBUG)
          Logger.debug("[unroll]  unable to unroll loop: " + msg)
        return node
    }

    val oldBody = Duplicate(loop.body) // duplicate for later use in post loop
    loop.begin = new VariableDeclarationStatement(IntegerDatatype, itVar, Unrolling.startVarAcc)
    loop.end = new LowerExpression(itVarAcc, Unrolling.intermVarAcc)
    loop.inc = new AssignmentStatement(itVarAcc, IntegerConstant(newStride), "+=")
    // duplicate private vars would also be possible...
    val interleave : Boolean = Knowledge.opt_unroll_interleave && loop.isParallel && loop.privateVars.isEmpty
    loop.body = duplicateStmts(loop.body, Knowledge.opt_unroll, itVar, oldStride, interleave)

    val annot = loop.removeAnnotation(Unrolling.UNROLLED_ANNOT)
    val unrolled : Boolean = annot.isDefined
    var res : ListBuffer[Statement] = null
    var intermDecl : VariableDeclarationStatement = null
    var postLoop : Statement = null
    if (unrolled) {
      res = new ListBuffer[Statement]()
      intermDecl = annot.get.value.asInstanceOf[VariableDeclarationStatement]
      intermDecl.expression = Some(Unrolling.getIntermExpr(newStride))
    } else {
      val (boundsDecls, postLoop_) : (ListBuffer[Statement], Statement) =
        Unrolling.getBoundsDeclAndPostLoop(itVar, start, endExcl, oldStride, oldBody, Duplicate(loop.reduction))
      postLoop = postLoop_
      intermDecl = Unrolling.getIntermDecl(newStride)
      res = boundsDecls += intermDecl
    }
    res += loop
    if (!unrolled)
      res += postLoop

    loop.annotate(SKIP_ANNOT)
    loop.annotate(Unrolling.UNROLLED_ANNOT, intermDecl)
    if (unrolled)
      return res
    else
      return new Scope(res)
  }

  private[optimization] def extractBoundsAndIncrement(begin : Statement, end : Expression, inc : Statement) : (String, Expression, Expression, Long) = {

    val (itVar, stride) = inc match {
      case ExpressionStatement(PreIncrementExpression(VariableAccess(itVar, Some(IntegerDatatype))))      => (itVar, 1L)
      case ExpressionStatement(PostIncrementExpression(VariableAccess(itVar, Some(IntegerDatatype))))     => (itVar, 1L)
      case AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype)), IntegerConstant(incr), "+=") => (itVar, incr)

      case AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype)),
        AdditionExpression(ListBuffer(VariableAccess(itVar2, Some(IntegerDatatype)), IntegerConstant(incr))),
        "=") if (itVar == itVar2) =>
        (itVar, incr)

      case AssignmentStatement(VariableAccess(itVar, Some(IntegerDatatype)),
        AdditionExpression(ListBuffer(IntegerConstant(incr), VariableAccess(itVar2, Some(IntegerDatatype)))),
        "=") if (itVar == itVar2) =>
        (itVar, incr)

      case _ => throw new UnrollException("cannot determine stride or it is negative:  " + inc.prettyprint())
    }
    if (stride <= 0)
      throw new UnrollException("loop stride must be positive:  " + inc.prettyprint())

    val lower : Expression =
      begin match {
        case VariableDeclarationStatement(IntegerDatatype, itVar2, Some(init)) if (itVar == itVar2) => init
        case _ => throw new UnrollException("cannot interpret loop begin: " + begin.prettyprint())
      }

    val upperExcl : Expression =
      end match {
        case LowerExpression(VariableAccess(itVar2, Some(IntegerDatatype)), bound) if (itVar == itVar2) => bound
        case LowerEqualExpression(VariableAccess(itVar2, Some(IntegerDatatype)), bound) if (itVar == itVar2) => new AdditionExpression(bound, IntegerConstant(1))
        case _ => throw new UnrollException("cannot interpret loop end: " + end.prettyprint())
      }

    return (itVar, lower, upperExcl, stride)
  }

  private def duplicateStmts(body : ListBuffer[Statement], unrollFactor : Int,
    itVar : String, oldInc : Long, interleave : Boolean) : ListBuffer[Statement] = {

    var njuBody : ListBuffer[Statement] = null
    val replaceStrat = new UpdateLoopVarAndNames(itVar)
    val dups = new ListBuffer[Iterator[Statement]]()

    for (i <- 1L until unrollFactor) {
      val dup = Duplicate(body)
      replaceStrat.offset = i * oldInc
      replaceStrat.applyStandalone(dup)
      SimplifyExpression.SimplifyIndices.applyStandalone(dup)
      dups += dup.iterator.filter(s => !s.isInstanceOf[CommentStatement])
    }

    if (interleave) {
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

  private[optimization] class UpdateLoopVarAndNames(itVar : String)
      extends QuietDefaultStrategy("Add loop var offset, rename declarations, and/or add annotation") {

    private final val SKIP_ANNOT = "UpLVSkip"
    private val rename = new HashSet[String]()

    var offset : Long = 0

    this += new Transformation("apply", {
      case vAcc @ VariableAccess(v, Some(IntegerDatatype)) if (v == itVar) =>
        if (offset != 0 && !vAcc.removeAnnotation(SKIP_ANNOT).isDefined) {
          vAcc.annotate(SKIP_ANNOT) // already done
          new AdditionExpression(vAcc, IntegerConstant(offset))
        } else
          vAcc

      case loop @ ForLoopStatement(decl : VariableDeclarationStatement, _, _, _, _) if (offset > 0) => // HACK: declaration must be handled first
        rename.add(decl.name)
        loop

      case vAcc : VariableAccess if (rename.contains(vAcc.name)) =>
        vAcc.name += "_" + offset
        vAcc

      case decl : VariableDeclarationStatement if (offset > 0) =>
        rename.add(decl.name)
        decl.name += "_" + offset
        decl
    })
  }
}
