package exastencils.optimization

import scala.collection.immutable.StringLike
import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.ir._
import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.iv.FieldData
import exastencils.logger._
import exastencils.util._

object AddressPrecalculation extends CustomStrategy("Perform address precalculation") {

  private[optimization] final val ORIG_IND_ANNOT = "AP_OrInd"

  private[optimization] final val DECLS_ANNOT = "Decls"
  private[optimization] final val REPL_ANNOT = "Replace"

  override def apply() : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    val annotate = new AnnotateLoopsAndAccesses()
    this.register(annotate)
    this.execute(new Transformation("Find relevant loops and accesses", PartialFunction.empty))
    this.unregister(annotate)

    this.execute(new Transformation("Optimize", IntegrateAnnotations))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }
}

private final class ArrayBases(val arrayName : String) {

  private val inits = new HashMap[HashMap[IR_Expression, Long], (String, IR_Expression)]()
  private var idCount = -1

  def getName(initVec : HashMap[IR_Expression, Long], base : IR_Expression, al : Boolean) : String = {
    inits.getOrElseUpdate(initVec, { idCount += 1; (arrayName + "_p" + idCount, new ArrayAccess(base, SimplifyExpression.recreateExprFromIntSum(initVec), al)) })._1
  }

  def addToDecls(decls : ListBuffer[IR_Statement]) : Unit = {
    for ((name : String, init : IR_Expression) <- inits.values.toArray.sortBy(_._1))
      decls += new VariableDeclarationStatement(IR_ConstPointerDatatype(IR_RealDatatype), name, IR_AddressofExpression(init))
  }
}

private final class AnnotateLoopsAndAccesses extends Collector {

  import AddressPrecalculation._

  private def generateName(expr : IR_Expression) : String = {
    return filter('_' + expr.prettyprint())
  }

  private def filter(cpp : StringLike[_]) : String = {
    val res = new StringBuilder()
    for (c : Char <- cpp)
      c match {
        case '[' | '.' | '%' | '+' | '-' => res.append('_')
        case ']' | '(' | ')' | ' '       =>
        case _                           => res.append(c)
      }
    return res.toString()
  }

  def containsLoopVar(expr : IR_Expression, allowed : String = null) : Boolean = {
    object Search extends QuietDefaultStrategy("Anonymous search") {
      var res : Boolean = false
      var allowed : String = null
      this += new Transformation("contains loop var", {
        case strC : IR_StringLiteral =>
          val name = strC.value
          res |= (allowed != name) && inVars.contains(name)
          strC
        case varA : VariableAccess   =>
          val name = varA.name
          res |= (allowed != name) && inVars.contains(name)
          varA
        case i : iv.InternalVariable =>
          val name = i.resolveName
          res |= (allowed != name) && inVars.contains(name)
          i
      })
    }
    Search.res = false
    Search.allowed = allowed
    Search.applyStandalone(new ReturnStatement(expr)) // wrap to ensure ALL nodes of expr are visited
    return Search.res
  }

  private def splitIndex(ind : IR_Expression) : (IR_Expression, HashMap[IR_Expression, Long]) = {

    val outMap = new HashMap[IR_Expression, Long]
    if (inVars == null)
      return (ind, outMap)

    // TODO: add support for MultiIndexExpression?
    val inMap : HashMap[IR_Expression, Long] =
    try {
      SimplifyExpression.extractIntegralSum(ind)
    } catch {
      case ex : EvaluationException =>
        var cause : Throwable = ex
        while (cause.getCause() != null)
          cause = cause.getCause()
        val stackTraceHead = cause.getStackTrace()(0)
        Logger.dbg("[APC]  cannot deal with index expression  (" + ex.msg + ")  in  " + ind.prettyprint() +
          "  (" + stackTraceHead.getFileName() + ':' + stackTraceHead.getLineNumber() + ')')
        return (ind, outMap)
    }

    // constant part should stay inside the loop, as this reduces the number of required pointers outside
    for ((expr, value) <- inMap)
      if (expr != SimplifyExpression.constName && !containsLoopVar(expr))
        outMap.put(expr, value)
    for ((expr, _) <- outMap)
      inMap.remove(expr)

    return (SimplifyExpression.recreateExprFromIntSum(inMap), outMap)
  }

  private final val SKIP_SUBTREE_ANNOT = "APCSST"
  private var skipSubtree : Boolean = false

  private var decls : HashMap[String, ArrayBases] = null
  private var inVars : Set[String] = null
  private val toAnalyze = new ListBuffer[ArrayAccess]()

  override def enter(node : Node) : Unit = {

    if (skipSubtree)
      return

    node match {
      case l : ForLoopStatement with OptimizationHint if (l.isInnermost) =>
        if (decls != null) {
          Logger.dbg("ups, nested \"innermost\" loops... something is wrong here")
          decls = null
          return
        }
        val d = new HashMap[String, ArrayBases]()
        l.inc match { // TODO: remove StringLiteral
          case AssignmentStatement(VariableAccess(name, _), _, _)                          =>
            decls = d
            inVars = Set(name)
          case AssignmentStatement(IR_StringLiteral(name), _, _)                           =>
            decls = d
            inVars = Set(name)
          case IR_ExpressionStatement(IR_PreIncrementExpression(VariableAccess(name, _)))  =>
            decls = d
            inVars = Set(name)
          case IR_ExpressionStatement(IR_PreIncrementExpression(IR_StringLiteral(name)))   =>
            decls = d
            inVars = Set(name)
          case IR_ExpressionStatement(IR_PostIncrementExpression(VariableAccess(name, _))) =>
            decls = d
            inVars = Set(name)
          case IR_ExpressionStatement(IR_PostIncrementExpression(IR_StringLiteral(name)))  =>
            decls = d
            inVars = Set(name)
          case IR_ExpressionStatement(IR_PreDecrementExpression(VariableAccess(name, _)))  =>
            decls = d
            inVars = Set(name)
          case IR_ExpressionStatement(IR_PreDecrementExpression(IR_StringLiteral(name)))   =>
            decls = d
            inVars = Set(name)
          case IR_ExpressionStatement(IR_PostDecrementExpression(VariableAccess(name, _))) =>
            decls = d
            inVars = Set(name)
          case IR_ExpressionStatement(IR_PostDecrementExpression(IR_StringLiteral(name)))  =>
            decls = d
            inVars = Set(name)
          case _                                                                           =>
            Logger.dbg("[addr precalc]  cannot determine loop variable name, inc of ForLoopStatement is not recognized:  " + l.inc)
            decls = d
        }
        node.annotate(DECLS_ANNOT, d)

      // ArrayAccess with a constant index only cannot be optimized further
      case acc : ArrayAccess if (decls != null && !acc.index.isInstanceOf[IR_IntegerConstant]) =>
        acc.annotate(SKIP_SUBTREE_ANNOT) // skip other ArrayAccesses below this one
        skipSubtree = true
        toAnalyze += acc

      case AssignmentStatement(dst, _, _) if (decls != null && inVars != null) =>
        dst match {
          case _ : IR_StringLiteral
               | _ : VariableAccess
               | _ : ArrayAccess
               | _ : iv.InternalVariable => inVars += resolveName(dst)
          case _                         => // nothing; expand match here, if more vars should stay inside the loop
        }

      case VariableDeclarationStatement(_, name, _) if (decls != null && inVars != null) =>
        inVars += name

      case _ => // ignore
    }
  }

  override def leave(node : Node) : Unit = {

    if (node.removeAnnotation(SKIP_SUBTREE_ANNOT).isDefined)
      skipSubtree = false

    node match {
      case l : ForLoopStatement with OptimizationHint if (l.isInnermost) =>
        // if base is ArrayAccess we ensure that it does not contain anything, which is written in the loop
        //   (the name of this access itself is not critical, see AssignmentStatement match in enter(..))
        for (acc @ ArrayAccess(base, index, al) <- toAnalyze) if (!containsLoopVar(base, resolveName(base))) {
          val (in : IR_Expression, outMap : HashMap[IR_Expression, Long]) = splitIndex(index)
          // if (!outMap.isEmpty) {
          var name : String = generateName(base)
          val bases : ArrayBases = decls.getOrElseUpdate(name, new ArrayBases(name))
          name = bases.getName(outMap, base, al)
          val datatype : Option[IR_Datatype] = base match {
            case fd : FieldData => Some(IR_ConstPointerDatatype(fd.field.resolveDeclType))
            case _              => None
          }
          val newAcc = new ArrayAccess(new VariableAccess(name, datatype), in, al)
          newAcc.annotate(ORIG_IND_ANNOT, Duplicate(index)) // save old (complete) index expression for vectorization
          acc.annotate(REPL_ANNOT, newAcc)
          // }
        }
        decls = null
        inVars = null
        toAnalyze.clear()
      case _                                                             => // ignore
    }
  }

  override def reset() : Unit = {
    skipSubtree = false
    decls = null
    inVars = null
    toAnalyze.clear()
  }

  private def resolveName(expr : IR_Expression) : String = {
    expr match {
      case ArrayAccess(base, _, _) => resolveName(base)
      case VariableAccess(name, _) => name
      case IR_StringLiteral(str)   => str
      case i : iv.InternalVariable => i.resolveName
    }
  }
}

private final object IntegrateAnnotations extends PartialFunction[Node, Transformation.OutputType] {

  import AddressPrecalculation._

  def isDefinedAt(node : Node) : Boolean = {
    return node.hasAnnotation(DECLS_ANNOT) || node.hasAnnotation(REPL_ANNOT)
  }

  def apply(node : Node) : Transformation.OutputType = {

    val repl = node.removeAnnotation(REPL_ANNOT)
    if (repl.isDefined)
      return repl.get.asInstanceOf[Node]

    val decls = node.removeAnnotation(DECLS_ANNOT).get.asInstanceOf[HashMap[String, ArrayBases]]
    if (decls.isEmpty)
      return node

    val stmts = new ListBuffer[IR_Statement]()
    for ((_, bases : ArrayBases) <- decls.toArray.sortBy(_._1))
      bases.addToDecls(stmts)

    stmts += node.asInstanceOf[IR_Statement]
    return new Scope(stmts)
  }
}
