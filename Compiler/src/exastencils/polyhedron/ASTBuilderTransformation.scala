package exastencils.polyhedron

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.TreeSet

import exastencils.core.Duplicate
import exastencils.core.Logger
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.optimization.OptimizationHint
import isl.Conversions._

class ASTBuilderTransformation(replaceCallback : (HashMap[String, Expression], Node) => Unit)
  extends Transformation("insert optimized loop AST", new ASTBuilderFunction(replaceCallback))

private final class ASTBuilderFunction(replaceCallback : (HashMap[String, Expression], Node) => Unit)
    extends PartialFunction[Node, Transformation.Output[_]] {

  private final val ZERO_VAL : isl.Val = isl.Val.zero()
  private final val ONE_VAL : isl.Val = isl.Val.one()

  private val loopStmts = new HashMap[String, ListBuffer[ForLoopStatement with OptimizationHint]]()
  private var oldStmts : HashMap[String, (Statement, ArrayBuffer[String])] = null
  private var seqDims : TreeSet[String] = null
  private var parallelize_omp : Boolean = false
  private var reduction : Option[Reduction] = None

  def isDefinedAt(node : Node) : Boolean = node match {
    case loop : LoopOverDimensions with PolyhedronAccessable =>
      loop.hasAnnotation(PolyOpt.SCOP_ANNOT)
    case _ => false
  }

  def apply(node : Node) : Transformation.Output[_] = {

    val scop : Scop = node.removeAnnotation(PolyOpt.SCOP_ANNOT).get.value.asInstanceOf[Scop]
    if (scop.remove)
      return NullStatement
    reduction = scop.reduction

    // find all sequential loops
    parallelize_omp = scop.parallelize
    seqDims = new TreeSet[String]()
    for (i <- scop.noParDims)
      seqDims += scop.njuLoopVars(i)

    var deps : isl.UnionMap = scop.deps.validity()
    deps = deps.applyDomain(scop.schedule)
    deps = deps.applyRange(scop.schedule)

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
    var itersId : isl.IdList = isl.IdList.alloc(dims)
    for (i <- 0 until dims)
      itersId = itersId.add(isl.Id.alloc(scop.njuLoopVars(i), null))

    loopStmts.clear()
    oldStmts = scop.stmts

    // build AST
    var islBuild : isl.AstBuild = isl.AstBuild.fromContext(scop.domain.params())
    islBuild = islBuild.setOptions(isl.UnionMap.readFromStr(options.toString()))
    islBuild = islBuild.setIterators(itersId)
    var scattering : isl.UnionMap = Isl.simplify(scop.schedule.intersectDomain(scop.domain))
    val islNode : isl.AstNode = islBuild.astFromSchedule(scattering)
    var nju : Statement =
      try {
        processIslNode(islNode)
      } catch {
        case PolyASTBuilderException(msg) =>
          Logger.debug("[poly ast] cannot create AST from model:  " + msg)
          // remove all annotations for the merged scops, as they are invalid now
          var s : Scop = scop.nextMerge
          while (s != null) {
            s.root.removeAnnotation(PolyOpt.SCOP_ANNOT)
            s = s.nextMerge
          }
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
    if (!scop.decls.isEmpty) {
      val scopeList = new ListBuffer[Statement]
      for (decl : VariableDeclarationStatement <- scop.decls) {
        decl.expression = None
        if (!scopeList.contains(decl)) // only add if not yet available
          scopeList += decl
      }
      scopeList += comment
      scopeList += nju
      return new Scope(scopeList)
    } else
      return ListBuffer[Statement](comment, nju)
  }

  private def processIslNode(node : isl.AstNode) : Statement = {

    return node.getType() match {

      case isl.AstNodeType.NodeFor =>
        if (node.forIsDegenerate()) {
          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val decl : Statement = VariableDeclarationStatement(IntegerDatatype(), islIt.getId().getName(), Some(processIslExpr(node.forGetInit())))

          Scope(ListBuffer[Statement](decl, processIslNode(node.forGetBody())))

        } else {
          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val itStr : String = islIt.getId().getName()
          val parOMP : Boolean = parallelize_omp && !seqDims.contains(itStr)
          parallelize_omp &= !parOMP // if code must be parallelized, then now (parNow) XOR later (parallelize)
          val it : VariableAccess = VariableAccess(itStr, Some(IntegerDatatype()))
          val init : Statement = VariableDeclarationStatement(IntegerDatatype(), itStr, Some(processIslExpr(node.forGetInit())))
          val cond : Expression = processIslExpr(node.forGetCond())
          val incr : Statement = AssignmentStatement(it, processIslExpr(node.forGetInc()), "+=")

          val body : Statement = processIslNode(node.forGetBody())
          parallelize_omp |= parOMP // restore overall parallelization level
          val loop : ForLoopStatement with OptimizationHint =
            if (parOMP)
              body match {
                case Scope(raw) => new ForLoopStatement(init, cond, incr, raw, reduction) with OptimizationHint with OMP_PotentiallyParallel
                case _          => new ForLoopStatement(init, cond, incr, body, reduction) with OptimizationHint with OMP_PotentiallyParallel
              }
            else
              body match {
                case Scope(raw) => new ForLoopStatement(init, cond, incr, raw, reduction) with OptimizationHint
                case _          => new ForLoopStatement(init, cond, incr, body, reduction) with OptimizationHint
              }
          loop.isParallel = seqDims != null && !seqDims.contains(itStr)
          loopStmts.getOrElseUpdate(itStr, new ListBuffer()) += loop
          loop
        }

      case isl.AstNodeType.NodeIf =>
        val cond : Expression = processIslExpr(node.ifGetCond())
        val thenBranch : Statement = processIslNode(node.ifGetThen())
        if (node.ifHasElse()) {
          val els : Statement = processIslNode(node.ifGetElse())
          new ConditionStatement(cond, thenBranch, els)
        } else
          new ConditionStatement(cond, thenBranch)

      case isl.AstNodeType.NodeBlock =>
        val stmts = new ListBuffer[Statement]
        node.blockGetChildren().foreach({ stmt : isl.AstNode => stmts += processIslNode(stmt); () })
        if (stmts.length == 1)
          stmts(0)
        else
          new Scope(stmts)

      case isl.AstNodeType.NodeUser =>
        val expr : isl.AstExpr = node.userGetExpr()
        assume(expr.getOpType() == isl.AstOpType.OpCall, "user node is no OpCall?!")
        val args : Array[Expression] = processArgs(expr)
        val name : String = args(0).asInstanceOf[StringConstant].value
        val (oldStmt : Statement, loopVars : ArrayBuffer[String]) = oldStmts(name)
        val stmt : Statement = Duplicate(oldStmt)
        val repl = new HashMap[String, Expression]()
        for (d <- 1 until args.length)
          repl.put(loopVars(loopVars.size - d), args(d))

        replaceCallback(repl, stmt)
        stmt

      case isl.AstNodeType.NodeError => throw new PolyASTBuilderException("NodeError found...")
    }
  }

  private def processIslExpr(expr : isl.AstExpr) : Expression = {

    return expr.getType() match { // TODO: check if ExprId contains only variable identifier
      case isl.AstExprType.ExprId =>
        val id : String = expr.getId().getName()
        ScopNameMapping.id2expr(id).getOrElse(StringConstant(id))
      case isl.AstExprType.ExprInt   => IntegerConstant(expr.getVal().toString().toLong)
      case isl.AstExprType.ExprOp    => processIslExprOp(expr)
      case isl.AstExprType.ExprError => throw new PolyASTBuilderException("ExprError found...")
    }
  }

  /** Process an isl.AstExpr of type isl.AstExprType.ExprOp. Caller must ensure only this type of node is passed! */
  private def processIslExprOp(expr : isl.AstExpr) : Expression = {

    val args : Array[Expression] = processArgs(expr)
    val n : Int = args.length

    return expr.getOpType() match {
      case isl.AstOpType.OpAndThen if n == 2 => AndAndExpression(args(0), args(1))
      case isl.AstOpType.OpAnd if n == 2     => AndAndExpression(args(0), args(1))
      case isl.AstOpType.OpOrElse if n == 2  => OrOrExpression(args(0), args(1))
      case isl.AstOpType.OpOr if n == 2      => OrOrExpression(args(0), args(1))
      case isl.AstOpType.OpMinus if n == 1   => UnaryExpression(UnaryOperators.Negative, args(0))
      case isl.AstOpType.OpAdd if n == 2     => AdditionExpression(args(0), args(1))
      case isl.AstOpType.OpSub if n == 2     => SubtractionExpression(args(0), args(1))
      case isl.AstOpType.OpMul if n == 2     => MultiplicationExpression(args(0), args(1))
      case isl.AstOpType.OpDiv if n == 2     => DivisionExpression(args(0), args(1))
      case isl.AstOpType.OpFdivQ if n == 2   => DivisionExpression(args(0), args(1)) // TODO: ensure integer division; round to -inf for negative
      case isl.AstOpType.OpPdivQ if n == 2   => DivisionExpression(args(0), args(1)) // TODO: ensure integer division
      case isl.AstOpType.OpPdivR if n == 2   => ModuloExpression(args(0), args(1))
      case isl.AstOpType.OpCond if n == 3    => TernaryConditionExpression(args(0), args(1), args(2))
      case isl.AstOpType.OpEq if n == 2      => EqEqExpression(args(0), args(1))
      case isl.AstOpType.OpLe if n == 2      => LowerEqualExpression(args(0), args(1))
      case isl.AstOpType.OpLt if n == 2      => LowerExpression(args(0), args(1))
      case isl.AstOpType.OpGe if n == 2      => GreaterEqualExpression(args(0), args(1))
      case isl.AstOpType.OpGt if n == 2      => GreaterExpression(args(0), args(1))
      case isl.AstOpType.OpMax if n >= 2     => MaximumExpression(ListBuffer(args : _*))
      case isl.AstOpType.OpMin if n >= 2     => MinimumExpression(ListBuffer(args : _*))

      case isl.AstOpType.OpCall if n >= 1 =>
        val fArgs = ListBuffer[Expression](args : _*)
        fArgs.remove(0)
        FunctionCallExpression(args(0), fArgs)

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
