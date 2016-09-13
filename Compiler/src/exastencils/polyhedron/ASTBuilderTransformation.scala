package exastencils.polyhedron

import scala.collection.mutable.{ ArrayBuffer, HashMap, ListBuffer, Map, Set }

import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.logger._
import exastencils.omp._
import exastencils.optimization._
import isl.Conversions._

class ASTBuilderTransformation(replaceCallback : (Map[String, IR_Expression], Node) => Unit)
  extends Transformation("insert optimized loop AST", new ASTBuilderFunction(replaceCallback))

private final class ASTBuilderFunction(replaceCallback : (Map[String, IR_Expression], Node) => Unit)
  extends PartialFunction[Node, Transformation.OutputType] {

  private final val ZERO_VAL : isl.Val = isl.Val.zero(Isl.ctx)
  private final val ONE_VAL : isl.Val = isl.Val.one(Isl.ctx)
  private final val NEG_ONE_VAL : isl.Val = isl.Val.negone(Isl.ctx)

  private val loopStmts = new HashMap[String, ListBuffer[OptimizationHint]]()
  private var oldStmts : HashMap[String, (ListBuffer[IR_Statement], ArrayBuffer[String])] = null
  private var parDims : Set[String] = null
  private var vecDims : Set[String] = null
  private var parallelize_omp : Boolean = false
  private var reduction : Option[Reduction] = None
  private var privateVars : ListBuffer[VariableAccess] = null
  private var condition : IR_Expression = null

  private def invalidateScop(scop : Scop) : Unit = {
    // remove all annotations for the merged scops, as they are invalid now
    var s : Scop = scop.nextMerge
    while (s != null) {
      s.root.removeAnnotation(PolyOpt.SCOP_ANNOT)
      s = s.nextMerge
    }
  }

  override def isDefinedAt(node : Node) : Boolean = node match {
    case loop : LoopOverDimensions with PolyhedronAccessible =>
      loop.hasAnnotation(PolyOpt.SCOP_ANNOT)
    case _                                                   => false
  }

  override def apply(node : Node) : Transformation.OutputType = {

    val loop = node.asInstanceOf[LoopOverDimensions]
    val scop : Scop = node.removeAnnotation(PolyOpt.SCOP_ANNOT).get.asInstanceOf[Scop]
    if (scop.remove)
      return IR_NullStatement
    reduction = scop.root.reduction
    condition = scop.root.condition.orNull

    // find all sequential loops
    parallelize_omp = scop.parallelize
    parDims = Set[String](scop.njuLoopVars : _*)
    vecDims = Set[String](scop.njuLoopVars : _*)
    for (i <- scop.noParDims)
      parDims -= scop.njuLoopVars(i)

    def respectDeps(deps : isl.UnionMap, forVect : Boolean) : Boolean = {
      val tDeps : isl.UnionMap = deps.intersectDomain(scop.domain).intersectRange(scop.domain).applyRange(scop.schedule).applyDomain(scop.schedule)
      tDeps.foreachMap({ dep : isl.Map =>
        val directions = dep.deltas()
        val universe : isl.Set = isl.BasicSet.universe(directions.getSpace())
        val dim : Int = universe.dim(isl.DimType.Set)
        for (i <- 0 until dim) {
          var seq = universe
          for (j <- 0 until i)
            seq = seq.fixVal(isl.DimType.Set, j, ZERO_VAL)
          seq = seq.lowerBoundVal(isl.DimType.Set, i, ONE_VAL)

          if (!seq.intersect(directions).isEmpty()) {
            val lVar = scop.njuLoopVars(i)
            parDims -= lVar
            if (forVect)
              vecDims -= lVar
          }

          seq = seq.dropConstraintsInvolvingDims(isl.DimType.Set, i, 1)
          seq = seq.upperBoundVal(isl.DimType.Set, i, NEG_ONE_VAL)

          val negative_deps = seq.intersect(directions)
          if (!negative_deps.isEmpty()) {
            Logger.debug("[poly ast] invalid dependence found (negative direction):  " + negative_deps)
            invalidateScop(scop)
            return false
          }
        }
      })
      return true
    }

    if (!respectDeps(scop.deps.validityParVec(), true))
      return node
    if (!respectDeps(scop.deps.validityPar(), false))
      return node

    // compute schedule dims
    var dims : Int = 0
    scop.schedule.foreachMap({
      sched : isl.Map => dims = math.max(dims, sched.dim(isl.DimType.Out))
    })

    // mark all additionally declared variables as private
    privateVars = new ListBuffer[VariableAccess]()
    for (VariableDeclarationStatement(dt, name, _) <- scop.decls)
      privateVars += VariableAccess(name, Some(dt))

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
    var islBuild : isl.AstBuild = isl.AstBuild.fromContext(scop.getContext())
    islBuild = islBuild.setOptions(isl.UnionMap.readFromStr(Isl.ctx, options.toString()))
    islBuild = islBuild.setIterators(itersId)
    val scattering : isl.UnionMap = Isl.simplify(scop.schedule.intersectDomain(scop.domain))
    val islNode : isl.AstNode = islBuild.astFromSchedule(scattering)
    var nju : ListBuffer[IR_Statement] =
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
      val innermostLoops = loopStmts.get(scop.njuLoopVars(i))
      if (innermostLoops.isDefined) {
        for (l <- innermostLoops.get)
          l.isInnermost = true
        i = -1 // break
      }
      i -= 1
    }

    // add comment (for debugging) and (eventually) declarations outside loop nest
    val comment = new CommentStatement("Statements in this Scop: " + scop.stmts.keySet.toArray.sorted.mkString(", "))
    comment +=: nju // prepend
    if (!scop.decls.isEmpty) {
      val scopeList = new ListBuffer[IR_Statement]
      for (decl : VariableDeclarationStatement <- scop.decls) {
        decl.expression = None
        if (!scopeList.contains(decl)) // only add if not yet available
          scopeList += decl
      }
      scopeList ++= nju
      return new Scope(loop.createOMPThreadsWrapper(scopeList))
    } else
      return loop.createOMPThreadsWrapper(nju)
  }

  private def processIslNode(node : isl.AstNode) : ListBuffer[IR_Statement] = {

    return node.getType() match {

      case isl.AstNodeType.NodeFor =>
        if (node.forIsDegenerate()) {
          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val decl : IR_Statement = new VariableDeclarationStatement(IR_IntegerDatatype, islIt.getId().getName(), processIslExpr(node.forGetInit()))
          processIslNode(node.forGetBody()).+=:(decl)

        } else {
          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val itStr : String = islIt.getId().getName()
          val parOMP : Boolean = parallelize_omp && parDims.contains(itStr)
          parallelize_omp &= !parOMP // if code must be parallelized, then now (parNow) XOR later (parallelize)
          val it : VariableAccess = new VariableAccess(itStr, IR_IntegerDatatype)
          val init : IR_Statement = new VariableDeclarationStatement(IR_IntegerDatatype, itStr, processIslExpr(node.forGetInit()))
          val cond : IR_Expression = processIslExpr(node.forGetCond())
          val incr : IR_Statement = new AssignmentStatement(it, processIslExpr(node.forGetInc()), "+=")

          val body : ListBuffer[IR_Statement] = processIslNode(node.forGetBody())
          parallelize_omp |= parOMP // restore overall parallelization level
          val loop : ForLoopStatement with OptimizationHint =
          if (parOMP)
            new ForLoopStatement(init, cond, incr, body, reduction) with OptimizationHint with OMP_PotentiallyParallel
          else
            new ForLoopStatement(init, cond, incr, body, reduction) with OptimizationHint
          loop.isParallel = parDims != null && parDims.contains(itStr)
          loop.isVectorizable = vecDims != null && vecDims.contains(itStr)
          loop.privateVars ++= privateVars
          loopStmts.getOrElseUpdate(itStr, new ListBuffer()) += loop
          ListBuffer[IR_Statement](loop)
        }

      case isl.AstNodeType.NodeIf =>
        val cond : IR_Expression = processIslExpr(node.ifGetCond())
        val thenBranch : ListBuffer[IR_Statement] = processIslNode(node.ifGetThen())
        if (node.ifHasElse()) {
          val els : ListBuffer[IR_Statement] = processIslNode(node.ifGetElse())
          ListBuffer[IR_Statement](new ConditionStatement(cond, thenBranch, els))
        } else
          ListBuffer[IR_Statement](new ConditionStatement(cond, thenBranch))

      case isl.AstNodeType.NodeBlock =>
        val stmts = new ListBuffer[IR_Statement]
        node.blockGetChildren().foreach({ stmt : isl.AstNode => stmts ++= processIslNode(stmt) })
        stmts

      case isl.AstNodeType.NodeUser =>
        val expr : isl.AstExpr = node.userGetExpr()
        assume(expr.getOpType() == isl.AstOpType.OpCall, "user node is no OpCall?!")
        val args : Array[IR_Expression] = processArgs(expr)
        val name : String = args(0).asInstanceOf[IR_StringLiteral].value
        val (oldStmt : ListBuffer[IR_Statement], loopVars : ArrayBuffer[String]) = oldStmts(name)
        val stmts : ListBuffer[IR_Statement] = Duplicate(oldStmt)
        val repl = new HashMap[String, IR_Expression]()
        for (d <- 1 until args.length)
          repl.put(loopVars(loopVars.size - d), args(d))

        replaceCallback(repl, Scope(stmts))
        if (condition != null)
          for (stmt <- stmts) {
            val cond : IR_Expression = Duplicate(condition)
            replaceCallback(repl, cond)
            stmt.annotate(PolyOpt.IMPL_CONDITION_ANNOT, cond)
          }
        stmts

      case isl.AstNodeType.NodeMark  => throw new PolyASTBuilderException("unexpected and unknown mark node found...")
      case isl.AstNodeType.NodeError => throw new PolyASTBuilderException("NodeError found...")
    }
  }

  private def processIslExpr(expr : isl.AstExpr) : IR_Expression = {

    return expr.getType() match { // TODO: check if ExprId contains only variable identifier
      case isl.AstExprType.ExprId    =>
        val id : String = expr.getId().getName()
        Duplicate(ScopNameMapping.id2expr(id)).getOrElse(IR_StringLiteral(id))
      case isl.AstExprType.ExprInt   => IR_IntegerConstant(expr.getVal().toString().toLong)
      case isl.AstExprType.ExprOp    => processIslExprOp(expr)
      case isl.AstExprType.ExprError => throw new PolyASTBuilderException("ExprError found...")
    }
  }

  /** Process an isl.AstExpr of type isl.AstExprType.ExprOp. Caller must ensure only this type of node is passed! */
  private def processIslExprOp(expr : isl.AstExpr) : IR_Expression = {

    val args : Array[IR_Expression] = processArgs(expr)
    val n : Int = args.length

    return expr.getOpType() match {
      case isl.AstOpType.OpEq if n == 2 && args(0).isInstanceOf[iv.NeighborIsValid] =>
        args(1) match {
          case IR_IntegerConstant(1) => args(0)
          case IR_IntegerConstant(0) => new IR_NegationExpression(args(0))
        }

      case isl.AstOpType.OpAndThen if n == 2 => IR_AndAndExpression(args(0), args(1))
      case isl.AstOpType.OpAnd if n == 2     => IR_AndAndExpression(args(0), args(1))
      case isl.AstOpType.OpOrElse if n == 2  => IR_OrOrExpression(args(0), args(1))
      case isl.AstOpType.OpOr if n == 2      => IR_OrOrExpression(args(0), args(1))
      case isl.AstOpType.OpMinus if n == 1   => IR_NegativeExpression(args(0))
      case isl.AstOpType.OpAdd if n == 2     => IR_AdditionExpression(args(0), args(1))
      case isl.AstOpType.OpSub if n == 2     => IR_SubtractionExpression(args(0), args(1))
      case isl.AstOpType.OpMul if n == 2     => IR_MultiplicationExpression(args(0), args(1))
      case isl.AstOpType.OpDiv if n == 2     => IR_DivisionExpression(args(0), args(1))
      case isl.AstOpType.OpFdivQ if n == 2   => new FunctionCallExpression("floord", args(0), args(1)) // TODO: ensure integer division
      case isl.AstOpType.OpPdivQ if n == 2   => IR_DivisionExpression(args(0), args(1)) // TODO: ensure integer division
      case isl.AstOpType.OpPdivR if n == 2   => IR_ModuloExpression(args(0), args(1))
      case isl.AstOpType.OpZdivR if n == 2   => IR_ModuloExpression(args(0), args(1)) // isl doc: Equal to zero iff the remainder on integer division is zero.
      case isl.AstOpType.OpCond if n == 3    => new TernaryConditionExpression(args(0), args(1), args(2))
      case isl.AstOpType.OpEq if n == 2      => IR_EqEqExpression(args(0), args(1))
      case isl.AstOpType.OpLe if n == 2      => IR_LowerEqualExpression(args(0), args(1))
      case isl.AstOpType.OpLt if n == 2      => IR_LowerExpression(args(0), args(1))
      case isl.AstOpType.OpGe if n == 2      => IR_GreaterEqualExpression(args(0), args(1))
      case isl.AstOpType.OpGt if n == 2      => IR_GreaterExpression(args(0), args(1))
      case isl.AstOpType.OpMax if n >= 2     => IR_MaximumExpression(args : _*)
      case isl.AstOpType.OpMin if n >= 2     => IR_MinimumExpression(args : _*)
      case isl.AstOpType.OpSelect if n == 3  => new TernaryConditionExpression(args(0), args(1), args(2))

      case isl.AstOpType.OpCall if n >= 1 =>
        val fArgs = ListBuffer[IR_Expression](args : _*)
        fArgs.remove(0)
        FunctionCallExpression(args(0).asInstanceOf[IR_StringLiteral].value, fArgs)

      case err =>
        throw new PolyASTBuilderException("expression not (yet) available:  " + err + "  with " + args.length + " arguments:  " + expr)
    }
  }

  private def processArgs(expr : isl.AstExpr) : Array[IR_Expression] = {

    val nArgs : Int = expr.getOpNArg()
    val args = new Array[IR_Expression](nArgs)
    for (i <- 0 until nArgs)
      args(i) = processIslExpr(expr.getOpArg(i))

    return args
  }
}

case class PolyASTBuilderException(msg : String = null) extends Exception(msg) {}
