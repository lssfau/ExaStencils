package exastencils.polyhedron

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.TreeSet

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.logger._
import exastencils.omp._
import exastencils.optimization._

import isl.Conversions._

class ASTBuilderTransformation(replaceCallback : (Map[String, Expression], Node) => Unit)
  extends Transformation("insert optimized loop AST", new ASTBuilderFunction(replaceCallback))

private final class ASTBuilderFunction(replaceCallback : (Map[String, Expression], Node) => Unit)
    extends PartialFunction[Node, Transformation.OutputType] {

  private final val ZERO_VAL : isl.Val = isl.Val.zero(Isl.ctx)
  private final val ONE_VAL : isl.Val = isl.Val.one(Isl.ctx)
  private final val NEG_ONE_VAL : isl.Val = isl.Val.negone(Isl.ctx)

  private val loopStmts = new HashMap[String, ListBuffer[OptimizationHint]]()
  private var oldStmts : HashMap[String, (ListBuffer[Statement], ArrayBuffer[String])] = null
  private var seqDims : TreeSet[String] = null
  private var parallelize_omp : Boolean = false
  private var reduction : Option[Reduction] = None
  private var condition : Expression = null

  private def invalidateScop(scop : Scop) : Unit = {
    // remove all annotations for the merged scops, as they are invalid now
    var s : Scop = scop.nextMerge
    while (s != null) {
      s.root.removeAnnotation(PolyOpt.SCOP_ANNOT)
      s = s.nextMerge
    }
  }

  override def isDefinedAt(node : Node) : Boolean = node match {
    case loop : LoopOverDimensions with PolyhedronAccessable =>
      loop.hasAnnotation(PolyOpt.SCOP_ANNOT)
    case _ => false
  }

  override def apply(node : Node) : Transformation.OutputType = {

    val scop : Scop = node.removeAnnotation(PolyOpt.SCOP_ANNOT).get.value.asInstanceOf[Scop]
    if (scop.remove)
      return NullStatement
    reduction = scop.root.reduction
    condition = scop.root.condition.getOrElse(null)

    // find all sequential loops
    parallelize_omp = scop.parallelize
    seqDims = new TreeSet[String]()
    for (i <- scop.noParDims)
      seqDims += scop.njuLoopVars(i)

    var deps : isl.UnionMap = scop.deps.validity()
    deps = deps.applyRange(scop.schedule)
    deps = deps.applyDomain(scop.schedule)

    deps.foreachMap({ dep : isl.Map =>
      val directions = dep.deltas()
      val universe : isl.Set = isl.BasicSet.universe(directions.getSpace())
      val dim : Int = universe.dim(isl.DimType.Set)
      for (i <- 0 until dim) {
        var seq = universe
        for (j <- 0 until i)
          seq = seq.fixVal(isl.DimType.Set, j, ZERO_VAL)
        seq = seq.lowerBoundVal(isl.DimType.Set, i, ONE_VAL)

        if (!seq.intersect(directions).isEmpty())
          seqDims += scop.njuLoopVars(i)

        seq = seq.dropConstraintsInvolvingDims(isl.DimType.Set, i, 1)
        seq = seq.upperBoundVal(isl.DimType.Set, i, NEG_ONE_VAL)

        val negative_deps = seq.intersect(directions)
        if (!negative_deps.isEmpty()) {
          Logger.debug("[poly ast] invalid dependence found (negative direction):  " + negative_deps)
          invalidateScop(scop)
          return node
        }
      }
    })

    // compute schedule dims
    var dims : Int = 0
    scop.schedule.foreachMap({
      sched : isl.Map => dims = math.max(dims, sched.dim(isl.DimType.Out))
    })

    // build AST generation options
    val options = new StringBuilder()
    options.append("[")
    if (dims > 0) {
      for (i <- 0 until dims)
        options.append('i').append(i).append(',')
      options.deleteCharAt(options.length - 1)
    }
    options.append(']')
    val scheduleDomain : String = options.toString()
    options.clear()
    options.append('{')
    // prevent guard inside loops
    options.append(scheduleDomain).append(" -> separate[xxx]")
    options.append('}')

    // build iterators list
    var itersId : isl.IdList = isl.IdList.alloc(Isl.ctx, dims)
    for (i <- 0 until dims)
      itersId = itersId.add(isl.Id.alloc(Isl.ctx, scop.njuLoopVars(i)))

    loopStmts.clear()
    oldStmts = scop.stmts

    // build AST
    var islBuild : isl.AstBuild = isl.AstBuild.fromContext(scop.domain.params())
    islBuild = islBuild.setOptions(isl.UnionMap.readFromStr(Isl.ctx, options.toString()))
    islBuild = islBuild.setIterators(itersId)
    var scattering : isl.UnionMap = Isl.simplify(scop.schedule.intersectDomain(scop.domain))
    val islNode : isl.AstNode = islBuild.astFromSchedule(scattering)
    var nju : ListBuffer[Statement] =
      try {
        processIslNode(islNode)
      } catch {
        case PolyASTBuilderException(msg) =>
          Logger.debug("[poly ast] cannot create AST from model:  " + msg)
          invalidateScop(scop)
          return node
      }

    // mark innermost loops
    var i : Int = scop.njuLoopVars.size - 1
    while (i >= 0) {
      var innermostLoops = loopStmts.get(scop.njuLoopVars(i))
      if (innermostLoops.isDefined) {
        for (l <- innermostLoops.get)
          l.isInnermost = true
        i = -1 // break
      }
      i -= 1
    }

    // add comment (for debugging) and (eventually) declarations outside loop nest
    val comment = new CommentStatement("Statements in this Scop: " + scop.stmts.keySet.mkString(", "))
    nju.+=:(comment) // "comment +=: nju" works too, but both versions are equally horrible
    if (!scop.decls.isEmpty) {
      val scopeList = new ListBuffer[Statement]
      for (decl : VariableDeclarationStatement <- scop.decls) {
        decl.expression = None
        if (!scopeList.contains(decl)) // only add if not yet available
          scopeList += decl
      }
      scopeList ++= nju
      return new Scope(scopeList)
    } else
      return nju
  }

  private def processIslNode(node : isl.AstNode) : ListBuffer[Statement] = {

    return node.getType() match {

      case isl.AstNodeType.For =>
        if (node.forIsDegenerate()) {
          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.Id, "isl for node iterator is not an ExprId")
          val decl : Statement = new VariableDeclarationStatement(IntegerDatatype, islIt.getId().getName(), processIslExpr(node.forGetInit()))
          processIslNode(node.forGetBody()).+=:(decl)

        } else {
          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.Id, "isl for node iterator is not an ExprId")
          val itStr : String = islIt.getId().getName()
          val parOMP : Boolean = parallelize_omp && !seqDims.contains(itStr)
          parallelize_omp &= !parOMP // if code must be parallelized, then now (parNow) XOR later (parallelize)
          val it : VariableAccess = new VariableAccess(itStr, IntegerDatatype)
          val init : Statement = new VariableDeclarationStatement(IntegerDatatype, itStr, processIslExpr(node.forGetInit()))
          val cond : Expression = processIslExpr(node.forGetCond())
          val incr : Statement = new AssignmentStatement(it, processIslExpr(node.forGetInc()), "+=")

          val body : ListBuffer[Statement] = processIslNode(node.forGetBody())
          parallelize_omp |= parOMP // restore overall parallelization level
          val loop : ForLoopStatement with OptimizationHint =
            if (parOMP)
              new ForLoopStatement(init, cond, incr, body, reduction) with OptimizationHint with OMP_PotentiallyParallel
            else
              new ForLoopStatement(init, cond, incr, body, reduction) with OptimizationHint
          loop.isParallel = seqDims != null && !seqDims.contains(itStr)
          loopStmts.getOrElseUpdate(itStr, new ListBuffer()) += loop
          ListBuffer[Statement](loop)
        }

      case isl.AstNodeType.If =>
        val cond : Expression = processIslExpr(node.ifGetCond())
        val thenBranch : ListBuffer[Statement] = processIslNode(node.ifGetThen())
        if (node.ifHasElse()) {
          val els : ListBuffer[Statement] = processIslNode(node.ifGetElse())
          ListBuffer[Statement](new ConditionStatement(cond, thenBranch, els))
        } else
          ListBuffer[Statement](new ConditionStatement(cond, thenBranch))

      case isl.AstNodeType.Block =>
        val stmts = new ListBuffer[Statement]
        node.blockGetChildren().foreach({ stmt : isl.AstNode => stmts ++= processIslNode(stmt) })
        stmts

      case isl.AstNodeType.User =>
        val expr : isl.AstExpr = node.userGetExpr()
        assume(expr.getOpType() == isl.AstOpType.Call, "user node is no OpCall?!")
        val args : Array[Expression] = processArgs(expr)
        val name : String = args(0).asInstanceOf[StringConstant].value
        val (oldStmt : ListBuffer[Statement], loopVars : ArrayBuffer[String]) = oldStmts(name)
        val stmts : ListBuffer[Statement] = Duplicate(oldStmt)
        val repl = new HashMap[String, Expression]()
        for (d <- 1 until args.length)
          repl.put(loopVars(loopVars.size - d), args(d))

        replaceCallback(repl, Scope(stmts))
        if (condition != null)
          for (stmt <- stmts) {
            val cond : Expression = Duplicate(condition)
            replaceCallback(repl, cond)
            stmt.annotate(PolyOpt.IMPL_CONDITION_ANNOT, cond)
          }
        stmts

      case isl.AstNodeType.Error => throw new PolyASTBuilderException("NodeError found...")
    }
  }

  private def processIslExpr(expr : isl.AstExpr) : Expression = {

    return expr.getType() match { // TODO: check if ExprId contains only variable identifier
      case isl.AstExprType.Id =>
        val id : String = expr.getId().getName()
        ScopNameMapping.id2expr(id).getOrElse(StringConstant(id))
      case isl.AstExprType.Int   => IntegerConstant(expr.getVal().toString().toLong)
      case isl.AstExprType.Op    => processIslExprOp(expr)
      case isl.AstExprType.Error => throw new PolyASTBuilderException("ExprError found...")
    }
  }

  /** Process an isl.AstExpr of type isl.AstExprType.ExprOp. Caller must ensure only this type of node is passed! */
  private def processIslExprOp(expr : isl.AstExpr) : Expression = {

    val args : Array[Expression] = processArgs(expr)
    val n : Int = args.length

    return expr.getOpType() match {
      case isl.AstOpType.AndThen if n == 2 => new AndAndExpression(args(0), args(1))
      case isl.AstOpType.And if n == 2     => new AndAndExpression(args(0), args(1))
      case isl.AstOpType.OrElse if n == 2  => new OrOrExpression(args(0), args(1))
      case isl.AstOpType.Or if n == 2      => new OrOrExpression(args(0), args(1))
      case isl.AstOpType.Minus if n == 1   => new NegativeExpression(args(0))
      case isl.AstOpType.Add if n == 2     => new AdditionExpression(args(0), args(1))
      case isl.AstOpType.Sub if n == 2     => new SubtractionExpression(args(0), args(1))
      case isl.AstOpType.Mul if n == 2     => new MultiplicationExpression(args(0), args(1))
      case isl.AstOpType.Div if n == 2     => new DivisionExpression(args(0), args(1))
      case isl.AstOpType.FdivQ if n == 2   => new FunctionCallExpression("floord", args(0), args(1)) // TODO: ensure integer division
      case isl.AstOpType.PdivQ if n == 2   => new DivisionExpression(args(0), args(1)) // TODO: ensure integer division
      case isl.AstOpType.PdivR if n == 2   => new ModuloExpression(args(0), args(1))
      case isl.AstOpType.ZdivR if n == 2   => new ModuloExpression(args(0), args(1)) // isl doc: Equal to zero iff the remainder on integer division is zero.
      case isl.AstOpType.Cond if n == 3    => new TernaryConditionExpression(args(0), args(1), args(2))
      case isl.AstOpType.Eq if n == 2      => new EqEqExpression(args(0), args(1))
      case isl.AstOpType.Le if n == 2      => new LowerEqualExpression(args(0), args(1))
      case isl.AstOpType.Lt if n == 2      => new LowerExpression(args(0), args(1))
      case isl.AstOpType.Ge if n == 2      => new GreaterEqualExpression(args(0), args(1))
      case isl.AstOpType.Gt if n == 2      => new GreaterExpression(args(0), args(1))
      case isl.AstOpType.Max if n >= 2     => new MaximumExpression(args : _*)
      case isl.AstOpType.Min if n >= 2     => new MinimumExpression(args : _*)
      case isl.AstOpType.Select if n == 3  => new TernaryConditionExpression(args(0), args(1), args(2))

      case isl.AstOpType.Call if n >= 1 =>
        val fArgs = ListBuffer[Expression](args : _*)
        fArgs.remove(0)
        FunctionCallExpression(args(0).asInstanceOf[StringConstant].value, fArgs)

      case err =>
        throw new PolyASTBuilderException("expression not (yet) available:  " + err + "  with " + args.length + " arguments:  " + expr)
    }
  }

  private def processArgs(expr : isl.AstExpr) : Array[Expression] = {

    val nArgs : Int = expr.getOpNArg()
    val args = new Array[Expression](nArgs)
    for (i <- 0 until nArgs)
      args(i) = processIslExpr(expr.getOpArg(i))

    return args
  }
}

case class PolyASTBuilderException(msg : String = null) extends Exception(msg) {}
