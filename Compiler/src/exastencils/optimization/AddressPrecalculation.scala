package exastencils.optimization

import scala.collection.immutable.StringLike
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
import exastencils.datastructures.ir.iv.FieldData
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.util.SimplifyExpression

object AddressPrecalculation extends CustomStrategy("Perform address precalculation") {

  private[optimization] final val DECLS_ANNOT = "Decls"
  private[optimization] final val REPL_ANNOT = "Replace"
  private[optimization] final val OMP_LOOP_ANNOT = "OMPLoop"

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    val annotate = new AnnotateLoopsAndAccesses()
    StateManager.register(annotate)
    this.execute(new Transformation("Find relevant loops and accesses", PartialFunction.empty))
    StateManager.unregister(annotate)

    this.execute(new Transformation("Optimize", IntegrateAnnotations))

    this.commit()
  }
}

private final class ArrayBases(val arrayName : String) {

  private val inits = new HashMap[HashMap[Expression, Long], (String, Expression)]()
  private var idCount = 0

  def getName(initVec : HashMap[Expression, Long], initExpr : => Expression) : String = {
    inits.getOrElseUpdate(initVec, { idCount += 1; (arrayName + "_p" + idCount, initExpr) })._1
  }

  def addToDecls(decls : ListBuffer[Node]) : Unit = {
    for ((_, (name : String, init : Expression)) <- inits)
      decls += new VariableDeclarationStatement(
        PointerDatatype(RealDatatype()), name, Some(new UnaryExpression(UnaryOperators.AddressOf, init)))
  }
}

private final class AnnotateLoopsAndAccesses extends Collector {
  import AddressPrecalculation._

  private val decls = new ArrayStack[(HashMap[String, ArrayBases], String)]()
  private var ompLoop : ForLoopStatement = null // save omp loop to lessen collapse value, if needed

  //  private def generateName(fa : LinearizedFieldAccess) : String = {
  //    return filter(FieldData(fa.fieldSelection.field, fa.fieldSelection.slot, fa.fieldSelection.fragIdx).cpp())
  //  }

  private def generateName(expr : Expression) : String = {
    val cpp = new StringBuilder()
    expr.cppsb(cpp)
    return filter(cpp)
  }

  private def filter(cpp : StringLike[_]) : String = {
    val res = new StringBuilder()
    for (c : Char <- cpp)
      c match {
        case '.' | '[' => res.append('_')
        case ']'       =>
        case _         => res.append(c)
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
        case AdditionExpression(l, r)                       => containsLoopVar(l) || containsLoopVar(r)
        case SubtractionExpression(l, r)                    => containsLoopVar(l) || containsLoopVar(r)
        case MultiplicationExpression(l, r)                 => containsLoopVar(l) || containsLoopVar(r)
        case DivisionExpression(l, r)                       => containsLoopVar(l) || containsLoopVar(r)
        case ModuloExpression(l, r)                         => containsLoopVar(l) || containsLoopVar(r)
        case UnaryExpression(UnaryOperators.Negative, expr) => containsLoopVar(expr)
        case StringConstant(str)                            => str == loopVar
        case VariableAccess(str, _)                         => str == loopVar
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
      case l : ForLoopStatement with OptimizationHint =>
        val d = new HashMap[String, ArrayBases]()
        l.begin match {
          case VariableDeclarationStatement(_, name, _) => decls.push((d, name))
          case _ =>
            Logger.dbg("[addr precalc]  cannot determine loop variable name, begin of ForLoopStatement is no VariableDeclarationStatement")
            decls.push((d, null))
        }
        node.annotate(DECLS_ANNOT, d)
        if (ompLoop == null && l.isInstanceOf[OMP_PotentiallyParallel])
          ompLoop = l
        node.annotate(OMP_LOOP_ANNOT, ompLoop)

      //      case f @ LinearizedFieldAccess(fieldSelection, index) if !decls.isEmpty =>
      //        var name : String = generateName(f)
      //        val (decl : HashMap[String, ArrayBases], loopVar) = decls.top
      //        val (in : Expression, outMap : HashMap[Expression, Long]) = splitIndex(index, loopVar)
      //        val bases : ArrayBases = decl.getOrElseUpdate(name, new ArrayBases(name))
      //        name = bases.getName(outMap,
      //          new LinearizedFieldAccess(fieldSelection, SimplifyExpression.recreateExpressionFromSum(outMap)))
      //        f.annotate(REPL_ANNOT, new ArrayAccess(new VariableAccess(name), in))

      // ArrayAccess with a constant index only cannot be optimized further
      case a @ ArrayAccess(base, index) if !decls.isEmpty && !index.isInstanceOf[IntegerConstant] =>
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
      case l : ForLoopStatement with OptimizationHint =>
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
  import AddressPrecalculation._

  def isDefinedAt(node : Node) : Boolean = {
    return node.hasAnnotation(DECLS_ANNOT) || node.hasAnnotation(REPL_ANNOT)
  }

  def apply(node : Node) : Transformation.Output[_] = {

    val annot = node.removeAnnotation(REPL_ANNOT)
    if (annot.isDefined)
      return annot.get.value.asInstanceOf[Node]

    else {
      val decls = node.removeAnnotation(DECLS_ANNOT).get.value
        .asInstanceOf[HashMap[String, ArrayBases]]
      if (decls.isEmpty)
        return node

      // lessen collapse, if available (we insert new statements and therefore the code is not perfectly nested anymore)
      val parLoop = node.removeAnnotation(OMP_LOOP_ANNOT).get.value.asInstanceOf[OMP_PotentiallyParallel]
      if (parLoop != null) parLoop.collapse = Math.max(1, parLoop.collapse - 1)

      val stmts = new ListBuffer[Node]()
      for ((_, bases : ArrayBases) <- decls)
        bases.addToDecls(stmts)

      stmts += node.asInstanceOf[Statement]
      return stmts
    }
  }
}
