package exastencils.optimization

import scala.collection.immutable.StringLike
import scala.collection.mutable.ArrayStack
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.iv.FieldData
import exastencils.logger._
import exastencils.util.SimplifyExpression

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

  private val inits = new HashMap[HashMap[Expression, Long], (String, Expression)]()
  private var idCount = 0

  def getName(initVec : HashMap[Expression, Long], base : Expression, al : Boolean) : String = {
    inits.getOrElseUpdate(initVec, { idCount += 1; (arrayName + "_p" + idCount, new ArrayAccess(base, SimplifyExpression.recreateExprFromIntSum(initVec), al)) })._1
  }

  def addToDecls(decls : ListBuffer[Statement]) : Unit = {
    for ((_, (name : String, init : Expression)) <- inits)
      decls += VariableDeclarationStatement(
        ConstPointerDatatype(RealDatatype()), name, Some(UnaryExpression(UnaryOperators.AddressOf, init)))
  }
}

private final class AnnotateLoopsAndAccesses extends Collector {
  import AddressPrecalculation._

  private def generateName(expr : Expression) : String = {
    return filter(expr.prettyprint())
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

  private def splitIndex(ind : Expression) : (Expression, HashMap[Expression, Long]) = {

    val outMap = new HashMap[Expression, Long]
    if (inVars == null)
      return (ind, outMap)

    val inMap : HashMap[Expression, Long] = SimplifyExpression.extractIntegralSum(ind)

    def containsLoopVar(expr : Expression) : Boolean = {
      var res : Boolean = false
      val contLoopVar = new DefaultStrategy("Anonymous") {
        this += new Transformation("contains loop var", {
          case strC : StringConstant =>
            res |= inVars.contains(strC.value)
            strC
          case varA : VariableAccess =>
            res |= inVars.contains(varA.name)
            varA
        })
      }
      val oldLvl = Logger.getLevel
      Logger.setLevel(Logger.WARNING)
      contLoopVar.applyStandalone(new ReturnStatement(Some(expr))) // wrap to ensure ALL nodes of expr are visited
      Logger.setLevel(oldLvl)
      return res
    }

    for ((expr, value) <- inMap)
      if (expr != SimplifyExpression.constName && !containsLoopVar(expr))
        outMap.put(expr, value)
    for ((expr, _) <- outMap)
      inMap.remove(expr)

    return (SimplifyExpression.recreateExprFromIntSum(inMap), outMap)
  }

  private var count : Int = 0

  private def checkId(
    curDecls : HashMap[String, (Expression, HashMap[Expression, Long])],
    name : String,
    outMap : HashMap[Expression, Long]) : String = {

    val d : Option[(Expression, HashMap[Expression, Long])] = curDecls.get(name)
    if (d.isEmpty || d.get._2 == outMap)
      return name

    val newName = "p_" + count
    count += 1

    return null
  }

  private var decls : HashMap[String, ArrayBases] = null
  private var inVars : Set[String] = null
  private val toAnalyze = new ListBuffer[ArrayAccess]()

  override def enter(node : Node) : Unit = {

    node match {
      case l : ForLoopStatement with OptimizationHint if (l.isInnermost) =>
        if (decls != null) {
          Logger.dbg("ups, nested \"innermost\" loops... something is wrong here")
          decls = null
          return
        }
        val d = new HashMap[String, ArrayBases]()
        l.inc match {
          case AssignmentStatement(VariableAccess(name, _), _, _) =>
            decls = d
            inVars = Set(name)
          case AssignmentStatement(StringConstant(name), _, _) =>
            decls = d
            inVars = Set(name)
          case _ =>
            Logger.dbg("[addr precalc]  cannot determine loop variable name, inc of ForLoopStatement is not recognized:  " + l.inc)
            decls = d
        }
        node.annotate(DECLS_ANNOT, d)

      // ArrayAccess with a constant index only cannot be optimized further
      case acc @ ArrayAccess(_, index, _) if (decls != null && !index.isInstanceOf[IntegerConstant]) =>
        toAnalyze += acc

      case ass : AssignmentStatement if (decls != null && inVars != null) =>
        ass.dest match {
          case StringConstant(name) => inVars += name
          case VariableAccess(name, _) => inVars += name
          case ArrayAccess(StringConstant(name), _, _) => inVars += name
          case ArrayAccess(VariableAccess(name, _), _, _) => inVars += name
          case _ => // nothing; expand match here, if more vars should stay inside the loop
        }

      case decl : VariableDeclarationStatement if (decls != null && inVars != null) =>
        inVars += decl.name

      case _ => // ignore
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case l : ForLoopStatement with OptimizationHint if (l.isInnermost) =>
        for (acc @ ArrayAccess(base, index, al) <- toAnalyze) {
          var name : String = generateName(base)
          val (in : Expression, outMap : HashMap[Expression, Long]) = splitIndex(index)
          val bases : ArrayBases = decls.getOrElseUpdate(name, new ArrayBases(name))
          name = bases.getName(outMap, base, al)
          val dType : Option[Datatype] = base match {
            case fd : FieldData => Some(ConstPointerDatatype(fd.field.dataType.resolveUnderlyingDatatype))
            case _              => None
          }
          val newAcc = new ArrayAccess(new VariableAccess(name, dType), in, al)
          newAcc.annotate(ORIG_IND_ANNOT, Duplicate(index))
          acc.annotate(REPL_ANNOT, newAcc)
        }
        decls = null
        inVars = null
        toAnalyze.clear()
      case _ => // ignore
    }
  }

  override def reset() : Unit = {
    decls = null
    inVars = null
    toAnalyze.clear()
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
      return repl.get.value.asInstanceOf[Node]

    val decls = node.removeAnnotation(DECLS_ANNOT).get.value.asInstanceOf[HashMap[String, ArrayBases]]
    if (decls.isEmpty)
      return node

    val stmts = new ListBuffer[Statement]()
    for ((_, bases : ArrayBases) <- decls)
      bases.addToDecls(stmts)

    stmts += node.asInstanceOf[Statement]
    return new Scope(stmts)
  }
}
