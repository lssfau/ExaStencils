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

  private val decls = new ArrayStack[(HashMap[String, ArrayBases], String)]()

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

  private def splitIndex(ind : Expression, loopVar : String) : (Expression, HashMap[Expression, Long]) = {

    val outMap = new HashMap[Expression, Long]
    if (loopVar == null)
      return (ind, outMap)

    val inMap : HashMap[Expression, Long] = SimplifyExpression.extractIntegralSum(ind)

    def containsLoopVar(expr : Expression) : Boolean = {
      var res : Boolean = false
      val contLoopVar = new DefaultStrategy("Anonymous") {
        this += new Transformation("contains loop var", {
          case strC : StringConstant =>
            res |= strC.value == loopVar
            strC
          case varA : VariableAccess =>
            res |= varA.name == loopVar
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

  override def enter(node : Node) : Unit = {

    node match {
      case l : ForLoopStatement with OptimizationHint if (l.isInnermost) =>
        val d = new HashMap[String, ArrayBases]()
        l.inc match {
          case AssignmentStatement(VariableAccess(name, _), _, _) => decls.push((d, name))
          case AssignmentStatement(StringConstant(name), _, _)    => decls.push((d, name))
          case _ =>
            Logger.dbg("[addr precalc]  cannot determine loop variable name, inc of ForLoopStatement is not recognized:  " + l.inc)
            decls.push((d, null))
        }
        node.annotate(DECLS_ANNOT, d)

      // ArrayAccess with a constant index only cannot be optimized further
      case a @ ArrayAccess(base, index, al) if (!decls.isEmpty && !index.isInstanceOf[IntegerConstant]) =>
        var name : String = generateName(base)
        val (decl : HashMap[String, ArrayBases], loopVar) = decls.top
        val (in : Expression, outMap : HashMap[Expression, Long]) = splitIndex(index, loopVar)
        val bases : ArrayBases = decl.getOrElseUpdate(name, new ArrayBases(name))
        name = bases.getName(outMap, base, al)
        val dType : Option[Datatype] =
          base match {
            case fd : FieldData => Some(ConstPointerDatatype(fd.field.dataType.resolveUnderlyingDatatype))
            case _              => None
          }
        a.annotate(REPL_ANNOT, new ArrayAccess(new VariableAccess(name, dType), in, al))

      case _ => // ignore
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case l : ForLoopStatement with OptimizationHint if (l.isInnermost) =>
        decls.pop()
      case _ => // ignore
    }
  }

  override def reset() : Unit = {
    decls.clear()
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
