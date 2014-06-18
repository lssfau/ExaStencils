package exastencils.optimization

import scala.collection.mutable.ArrayStack
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core.Logger
import exastencils.core.StateManager
import exastencils.core.collectors.Collector
import exastencils.datastructures.CustomStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.convFromNode
import exastencils.datastructures.ir.AdditionExpression
import exastencils.datastructures.ir.ArrayAccess
import exastencils.datastructures.ir.Datatype
import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.ForLoopStatement
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.LinearizedFieldAccess
import exastencils.datastructures.ir.ModuloExpression
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.PointerDatatype
import exastencils.datastructures.ir.RealDatatype
import exastencils.datastructures.ir.Scope
import exastencils.datastructures.ir.Statement
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.UnaryExpression
import exastencils.datastructures.ir.UnaryOperators
import exastencils.datastructures.ir.VariableAccess
import exastencils.datastructures.ir.VariableDeclarationStatement
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.util.SimplifyExpression

trait PrecalcAddresses

object AddressPrecalculation extends CustomStrategy("Perform address precalculation") {

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    StateManager.register(AnnotateLoopsAndAccesses)
    this.execute(new Transformation("Find relevant loops and accesses", PartialFunction.empty))
    StateManager.unregister(AnnotateLoopsAndAccesses)

    this.execute(new Transformation("Optimize", IntegrateAnnotations))

    this.commit()
  }
}

class ArrayBases(val arrayName : String) {
  private val inits = new HashMap[HashMap[Expression, Long], (String, Expression)]()
  private var idCount = 0

  def getName(initVec : HashMap[Expression, Long], initExpr : => Expression) : String = {
    inits.getOrElseUpdate(initVec, { idCount += 1; (arrayName + "_p" + idCount, initExpr) })._1
  }

  def getDecls(decls : ListBuffer[Statement]) : Unit = {
    for ((_, (name : String, init : Expression)) <- inits)
      decls += new VariableDeclarationStatement(
        PointerDatatype(RealDatatype()), name, Some(new UnaryExpression(UnaryOperators.AddressOf, init)))
  }
}

private final object AnnotateLoopsAndAccesses extends Collector {

  final val DECLS_ANNOT = "Decls"
  final val REPL_ANNOT = "Replace"
  final val OMP_LOOP_ANNOT = "OMPLoop"

  val decls = new ArrayStack[(HashMap[String, ArrayBases], String)]()
  var ompLoop : ForLoopStatement = null // save omp loop to lessen collapse value, if needed

  private def generateName(fa : LinearizedFieldAccess) : String = {
    val res = new StringBuilder()
    fa.fieldSelection.prefix.cppsb(res)
    res.append(fa.fieldSelection.codeName)
    res.append('_')
    fa.fieldSelection.slot.cppsb(res)
    var i : Int = 0
    while (i < res.size) {
      res(i) match {
        case '.' | '[' => res(i) = '_'
        case _         =>
      }
      i += 1
    }
    return res.toString()
  }

  private def generateName(expr : Expression) : String = {
    val res = new StringBuilder
    expr.cppsb(res)
    var i : Int = 0
    while (i < res.size) {
      res(i) match {
        case '.' | '[' => res(i) = '_'
        case _         =>
      }
      i += 1
    }
    return res.toString()
  }

  private def splitIndex(ind : Expression, loopVar : String) : (Expression, HashMap[Expression, Long]) = {

    val outMap = new HashMap[Expression, Long]
    if (loopVar == null)
      return (ind, outMap)

    val inMap : HashMap[Expression, Long] = SimplifyExpression.extractIntegralSum(ind)

    def containsLoopVar(expr : Expression) : Boolean =
      expr match {
        case AdditionExpression(l, r)       => containsLoopVar(l) || containsLoopVar(r)
        case SubtractionExpression(l, r)    => containsLoopVar(l) || containsLoopVar(r)
        case MultiplicationExpression(l, r) => containsLoopVar(l) || containsLoopVar(r)
        case DivisionExpression(l, r)       => containsLoopVar(l) || containsLoopVar(r)
        case ModuloExpression(l, r)         => containsLoopVar(l) || containsLoopVar(r)
        case StringConstant(str)            => str == loopVar
        case VariableAccess(str, _)         => str == loopVar
        case _ : Datatype
          | _ : IntegerConstant => false
      }

    for ((expr, value) <- inMap)
      if (expr != SimplifyExpression.constName && !containsLoopVar(expr))
        outMap.put(expr, value)
    for ((expr, _) <- outMap)
      inMap.remove(expr)

    return (SimplifyExpression.recreateExpressionFromSum(inMap), outMap)
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
      case l : ForLoopStatement with PrecalcAddresses =>
        val d = new HashMap[String, ArrayBases]()
        l.begin match {
          case VariableDeclarationStatement(_, name, _) => decls.push((d, name))
          case _                                        => decls.push((d, null))
        }
        node.annotate(DECLS_ANNOT, d)
        if (ompLoop == null && l.isInstanceOf[OMP_PotentiallyParallel])
          ompLoop = l
        node.annotate(OMP_LOOP_ANNOT, ompLoop)

      case f @ LinearizedFieldAccess(fieldSelection, index) if !decls.isEmpty =>
        var name : String = generateName(f)
        val (decl : HashMap[String, ArrayBases], loopVar) = decls.top
        val (in : Expression, outMap : HashMap[Expression, Long]) = splitIndex(index, loopVar)
        val bases : ArrayBases = decl.getOrElseUpdate(name, new ArrayBases(name))
        name = bases.getName(outMap,
          new LinearizedFieldAccess(fieldSelection, SimplifyExpression.recreateExpressionFromSum(outMap)))
        f.annotate(REPL_ANNOT, new ArrayAccess(new VariableAccess(name), in))

      case a @ ArrayAccess(base, index) if !decls.isEmpty =>
        var name : String = generateName(base)
        val (decl : HashMap[String, ArrayBases], loopVar) = decls.top
        val (in : Expression, outMap : HashMap[Expression, Long]) = splitIndex(index, loopVar)
        val bases : ArrayBases = decl.getOrElseUpdate(name, new ArrayBases(name))
        name = bases.getName(outMap,
          new ArrayAccess(base, SimplifyExpression.recreateExpressionFromSum(outMap)))
        a.annotate(REPL_ANNOT, new ArrayAccess(new VariableAccess(name), in))

      case _ => // ignore
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case l : ForLoopStatement with PrecalcAddresses =>
        if (l eq ompLoop)
          ompLoop = null
        decls.pop()
      case _ => // ignore
    }
  }

  override def reset() : Unit = {
    decls.clear()
  }
}

private final object IntegrateAnnotations extends PartialFunction[Node, Transformation.Output[_]] {

  def isDefinedAt(node : Node) : Boolean = {
    node.hasAnnotation(AnnotateLoopsAndAccesses.DECLS_ANNOT) || node.hasAnnotation(AnnotateLoopsAndAccesses.REPL_ANNOT)
  }

  def apply(node : Node) : Transformation.Output[_] = {

    val annot = node.removeAnnotation(AnnotateLoopsAndAccesses.REPL_ANNOT)
    if (annot.isDefined)
      return annot.get.value.asInstanceOf[Node]

    else {
      val decls = node.removeAnnotation(AnnotateLoopsAndAccesses.DECLS_ANNOT).get.value
        .asInstanceOf[HashMap[String, ArrayBases]]
      if (decls.isEmpty)
        return node

      // lessen collapse, if available (we insert new statements and therefore the code is not perfectly nested anymore)
      val parLoop : OMP_PotentiallyParallel = node.removeAnnotation(AnnotateLoopsAndAccesses.OMP_LOOP_ANNOT).get.value
        .asInstanceOf[OMP_PotentiallyParallel]
      if (parLoop != null) parLoop.collapse = Math.max(1, parLoop.collapse - 1)

      val stmts = new ListBuffer[Statement]()
      for ((_, bases : ArrayBases) <- decls)
        bases.getDecls(stmts)

      stmts += node.asInstanceOf[Statement]
      return new Scope(stmts)
    }
  }
}
