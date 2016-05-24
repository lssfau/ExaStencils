package exastencils.optimization

import java.util.IdentityHashMap
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Stack
import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.logger._
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.util._
import exastencils.omp.OMP_Lastprivate

object UnrollingCorrection extends CustomStrategy("Loop unrolling in correction code") {

  private[optimization] final val MOD_RESULT_ANNOT = "URCMR"

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    val fl = new FindLoops()
    this.register(fl)
    this.execute(new Transformation("find relevant loops", PartialFunction.empty))
    this.unregister(fl)

    val funcs = new IdentityHashMap[FunctionStatement, Any]() // hashCode changes during trafo, so use identity here
    for ((loop, funcStmt) <- fl.loops if (loop.inc.isInstanceOf[AssignmentStatement] && loop.inc.asInstanceOf[AssignmentStatement].dest.asInstanceOf[VariableAccess].name != "i1")) {
      funcs.put(funcStmt, null)
      this.execute(new Transformation("unroll", new UnrollFunction(loop)), Some(funcStmt))
    }
    //    for (i <- 0 until 3) { // TODO: determine which loops to unroll (according to vars in modulo expressions?)
    //      val (loop, funcStmt) = fl.loops.pop()
    //      funcs.put(funcStmt, null)
    //      this.execute(new Transformation("unroll", new UnrollFunction(loop)), Some(funcStmt))
    //    }

    for ((func, _) <- funcs) {
      new ResolveModulo().applyStandalone(func)
      SimplifyExpression.SimplifyIndices.applyStandalone(func)
      val dedup = new AnnotateDupDeclsAndReplAccs()
      this.register(dedup)
      this.execute(new Transformation("unify accesses", PartialFunction.empty), Some(func))
      this.unregister(dedup)
      this.execute(new Transformation("remove redundant declarations", RemoveAnnotatedNodes), Some(func))
    }

    this.commit()
  }
}

private class UnrollFunction(loop : ForLoopStatement) extends PartialFunction[Node, Transformation.OutputType] {

  private final val DEBUG = true

  private var first : Boolean = true
  override def isDefinedAt(node : Node) : Boolean = {
    if (!first)
      return false
    if (loop eq node) {
      first = false
      return true
    }
    return false
  }

  override def apply(node : Node) : Transformation.OutputType = {
    val loop = node.asInstanceOf[ForLoopStatement]

    var njuOffset : Long = 0
    var njuBegin : VariableDeclarationStatement = null
    var njuEnd : Expression = null
    var njuIncr : Statement = null
    var itVar : String = null
    var oldInc : Long = 0
    try {
      // do not modify original structure, as an exception could be thrown, which requires a rollback
//      val res : (Statement, String, Long) = UnrollInnermost.updateIncrement(loop.inc, 2)
//      njuIncr = res._1
//      itVar = res._2
//      oldInc = res._3
//      njuOffset = oldInc * 2 - 1
//      njuEnd = UnrollInnermost.updateEnd(loop.end, itVar, njuOffset)
//      njuBegin = UnrollInnermost.updateBegin(loop.begin, itVar)
      if (njuBegin == null)
        throw new UnrollException("cannot handle loop begin:  " + loop.begin.prettyprint())
    } catch {
      case UnrollException(msg) =>
        if (DEBUG)
          Logger.debug("[unroll correction]  unable to unroll loop: " + msg)
        return node
    }
    if (oldInc != 1L)
      return node

    def itVarExpr = new VariableAccess(itVar, IntegerDatatype)

    val res = new ListBuffer[Statement]()
    loop match {
      case parLoop : ForLoopStatement with OMP_PotentiallyParallel => // allow omp parallelization
        parLoop.additionalOMPClauses += new OMP_Lastprivate(new VariableAccess(itVar, Some(IntegerDatatype)))
      case _ =>
    }
    res += new VariableDeclarationStatement(IntegerDatatype, '_' + itVar + "_start", njuBegin.expression)
    njuBegin.expression = Some(new VariableAccess('_' + itVar + "_start", IntegerDatatype))
    res += Duplicate(njuBegin)
    val pre : ConditionStatement = new ConditionStatement(
      EqEqExpression(itVarExpr Mod IntegerConstant(2), IntegerConstant(1)),
      Duplicate(loop.body) += new AssignmentStatement(new VariableAccess('_' + itVar + "_start", IntegerDatatype), IntegerConstant(1), "+=")) // unroll factor == 2
    addModResAnnotation(pre.trueBody, itVar, 1L)
    res += pre
    res += loop // update fields later
    val post = new ConditionStatement(Duplicate(loop.end), Duplicate(loop.body))
    addModResAnnotation(post.trueBody, itVar, 0L)
    res += post
    // use loop, instead of a single condition to allow additional unrolling later
    //    res += new ForLoopStatement(NullStatement, Duplicate(loop.end), Duplicate(loop.inc), Duplicate(loop.body), Duplicate(loop.reduction))
    loop.begin = new AssignmentStatement(itVarExpr, njuBegin.expression.get)
    loop.end = njuEnd
    loop.inc = njuIncr
    loop.body = duplicateStmts(loop.body, itVar, oldInc)
    addModResAnnotation(loop.body, itVar, 0L)

    //    loop.annotate(Unrolling.NO_REM_ANNOT, -1L)
    //    loop.annotate(Unrolling.NO_REM_ANNOT, njuOffset)
    if (njuBegin != null)
      return new Scope(res)
    else
      return res
  }

  private def duplicateStmts(body : ListBuffer[Statement], itVar : String,
    oldInc : Long) : ListBuffer[Statement] = {

    val dup = Duplicate(body)

    val replaceStrat = new UnrollInnermost.UpdateLoopVarAndNames(itVar)
    replaceStrat.offset = oldInc
    replaceStrat.applyStandalone(Scope(dup))

    body ++= dup.iterator.filter(s => !s.isInstanceOf[CommentStatement])

    return body
  }

  private def addModResAnnotation(stmts : Seq[Statement], varName : String, modRes : Long) : Unit = {
    for (stmt <- stmts)
      stmt.annotate(UnrollingCorrection.MOD_RESULT_ANNOT, (varName, modRes))
  }
}

private class FindLoops extends Collector {

  var funcStmt : FunctionStatement = null
  val loops = new Stack[(ForLoopStatement, FunctionStatement)]

  override def enter(node : Node) : Unit = {
    node match {
      case f : FunctionStatement if (f.name.startsWith("Correction_8")) =>
        funcStmt = f
      case l : ForLoopStatement if (funcStmt != null) =>
        loops.push((l, funcStmt))
      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : FunctionStatement => funcStmt = null
      case _                     =>
    }
  }

  override def reset() : Unit = {
    funcStmt = null
    loops.clear()
  }
}

private class ResolveModulo extends DefaultStrategy("Resolve modulo expressions") {

  private val modResults = Map[String, Long]()
  private val collector = new Collector {
    override def enter(node : Node) : Unit = {
      val annot = node.getAnnotation(UnrollingCorrection.MOD_RESULT_ANNOT)
      if (annot.isDefined)
        modResults += annot.get.asInstanceOf[(String, Long)]
      //      val annotImpl : Option[Annotation] = node.getAnnotation(PolyOpt.IMPL_CONDITION_ANNOT)
      //      if (annotImpl.isDefined && annotImpl.get.value.isInstanceOf[EqEqExpression])
      //        integrateColorCond(annotImpl.get.value.asInstanceOf[EqEqExpression])
    }
    override def leave(node : Node) : Unit = {
      val annot = node.removeAnnotation(UnrollingCorrection.MOD_RESULT_ANNOT)
      if (annot.isDefined)
        modResults -= annot.get.asInstanceOf[(String, Long)]._1
    }
    override def reset() : Unit = {
      modResults.clear()
    }
  }

  this += new Transformation("apply", {
    case mod @ ModuloExpression(_, IntegerConstant(2)) =>
      try {
        val modResMap = integrateColorCond()
        val cName = SimplifyExpression.constName
        val sum : HashMap[Expression, Long] = SimplifyExpression.extractIntegralSum(mod.left)
        var const : Long = sum.remove(cName).getOrElse(0L)
        val njuSum = new HashMap[Expression, Long]()
        var allOrNothing : Boolean = false
        for (s @ (expr, coeff) <- sum) {
          allOrNothing |= coeff < 0
          expr match {
            case VariableAccess(name, _) =>
              val modRes : Option[Long] = modResMap.get(name)
              if (modRes.isDefined)
                const += coeff * modRes.get
              else
                njuSum += s
            case _ =>
              njuSum += s
          }
        }
        if (njuSum.isEmpty) new IntegerConstant(const % 2)
        else if (allOrNothing) mod
        else new ModuloExpression(SimplifyExpression.recreateExprFromIntSum(njuSum += ((cName, const % 2))), IntegerConstant(2))
      } catch {
        case _ : EvaluationException => mod // HACK...
      }
  })

  private def integrateColorCond() : Map[String, Long] = { // TODO: cache results? currently called for every ModuloExpr
    val (sum, cValue) : (Expression, Long) =
      ColorCondCollector.cond match {
        case EqEqExpression(IntegerConstant(c), ModuloExpression(sum, IntegerConstant(2))) => (sum, c)
        case EqEqExpression(ModuloExpression(sum, IntegerConstant(2)), IntegerConstant(c)) => (sum, c)
        case _ => return modResults
      }
    var const : Long = 0
    var remaining : String = null
    val sumMap = SimplifyExpression.extractIntegralSum(sum)
    if (sumMap.size == 1)
      return modResults
    for ((expr, coeff) <- sumMap) expr match {
      case VariableAccess(name, _) =>
        val modRes : Option[Long] = modResults.get(name)
        if (modRes.isDefined)
          const += coeff * modRes.get
        else if (remaining == null)
          remaining = name
        else
          return modResults
      case _ => return modResults
    }

    if (remaining == null)
      return modResults

    val modResExt = modResults.clone()
    modResExt(remaining) = ((cValue - const) % 2 + 2) % 2
    println("  old: " + modResults + ";   new: " + modResExt + ";   cond: " + ColorCondCollector.cond.prettyprint())
    return modResExt
  }

  override def applyStandalone(node : Node) : Unit = {
    val oldLvl = Logger.getLevel
    Logger.setLevel(1)
    this.register(collector) // TODO: merge both? (and enhance, ColorCond cannot deal with nested infos)
    this.register(ColorCondCollector)
    super.applyStandalone(node)
    this.unregister(collector)
    this.unregister(ColorCondCollector)
    Logger.setLevel(oldLvl)
  }
}

private class AnnotateDupDeclsAndReplAccs extends ScopeCollector((Map[Expression, String](), Map[String, String]())) {

  private def curInits : Map[Expression, String] = curScope._1
  private def curReplaces : Map[String, String] = curScope._2

  override def cloneCurScope() : (Map[Expression, String], Map[String, String]) = {
    return (curInits.clone(), curReplaces.clone())
  }

  override def enter(node : Node) : Unit = {
    super.enter(node)

    node match {
      case decl @ VariableDeclarationStatement(ConstPointerDatatype(_), name, Some(init)) =>
        val otherVar = curInits.get(init)
        if (otherVar.isDefined) {
          curReplaces(name) = otherVar.get
          decl.annotate(RemoveAnnotatedNodes.ANNOT)
        } else
          curInits(init) = name
      case vAcc @ VariableAccess(_, Some(ConstPointerDatatype(_))) =>
        val newName = curReplaces.get(vAcc.name)
        if (newName.isDefined)
          vAcc.name = newName.get
      // HACK: ensure the iterator declaration is visited before the body...
      case ForLoopStatement(begin, _, _, _, _) =>
        this.enter(begin)

      case _ =>
    }
  }
}

private object RemoveAnnotatedNodes extends PartialFunction[Node, Transformation.OutputType] {
  final val ANNOT : String = "remove"

  override def isDefinedAt(node : Node) : Boolean = {
    return node.hasAnnotation(ANNOT)
  }

  override def apply(node : Node) : Transformation.OutputType = {
    return NullStatement
  }
}
