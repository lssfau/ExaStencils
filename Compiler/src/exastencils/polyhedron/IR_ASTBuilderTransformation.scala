//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.polyhedron

import scala.collection.mutable.{ ArrayBuffer, HashMap, ListBuffer, Set }

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.parallelization.api.cuda.CUDA_Util
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.util.ir.IR_ReplaceVariableAccess

class IR_ASTBuilderTransformation()
  extends Transformation("insert optimized loop AST", new IR_ASTBuilderFunction())

private final class IR_ASTBuilderFunction()
  extends PartialFunction[Node, Transformation.OutputType] {

  private final val ZERO_VAL : isl.Val = isl.Val.zero(Isl.ctx)
  private final val ONE_VAL : isl.Val = isl.Val.one(Isl.ctx)
  private final val NEG_ONE_VAL : isl.Val = isl.Val.negone(Isl.ctx)

  private val loopStmts = new HashMap[String, ListBuffer[IR_ForLoop]]()
  private var oldStmts : HashMap[String, (ListBuffer[IR_Statement], ArrayBuffer[String])] = null
  private var parDims : Set[String] = null
  private var vecDims : Set[String] = null
  private var parallelize_omp : Boolean = false
  private var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()

  private def invalidateScop(scop : Scop) : Unit = {
    // remove all annotations for the merged scops, as they are invalid now
    var s : Scop = scop.nextMerge
    while (s != null) {
      s.root.removeAnnotation(IR_PolyOpt.SCOP_ANNOT)
      s = s.nextMerge
    }
  }

  override def isDefinedAt(node : Node) : Boolean = node match {
    case loop : IR_LoopOverDimensions => loop.hasAnnotation(IR_PolyOpt.SCOP_ANNOT)
    case _                            => false
  }

  override def apply(node : Node) : Transformation.OutputType = {

    val loop = node.asInstanceOf[IR_LoopOverDimensions]
    val scop : Scop = node.removeAnnotation(IR_PolyOpt.SCOP_ANNOT).get.asInstanceOf[Scop]
    if (scop.remove)
      return IR_NullStatement
    parallelization = Duplicate(scop.root.parallelization)

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
        val universe : isl.Set = isl.BasicSet.universe(directions.getSpace)
        val dim : Int = universe.dim(isl.DimType.Set)
        for (i <- 0 until dim) {
          var seq = universe
          for (j <- 0 until i)
            seq = seq.fixVal(isl.DimType.Set, j, ZERO_VAL)
          seq = seq.lowerBoundVal(isl.DimType.Set, i, ONE_VAL)

          if (!seq.intersect(directions).isEmpty) {
            val lVar = scop.njuLoopVars(i)
            parDims -= lVar
            if (forVect)
              vecDims -= lVar
          }

          seq = seq.dropConstraintsInvolvingDims(isl.DimType.Set, i, 1)
          seq = seq.upperBoundVal(isl.DimType.Set, i, NEG_ONE_VAL)

          val negative_deps = seq.intersect(directions)
          if (!negative_deps.isEmpty) {
            Logger.debug("[poly ast] invalid dependence found (negative direction):  " + negative_deps)
            invalidateScop(scop)
            return false
          }
        }
      })
      true
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
    for (IR_VariableDeclaration(dt, name, _, _) <- scop.decls)
      parallelization.privateVars += IR_VariableAccess(name, dt)

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
    // prevent guard inside loops for all except CUDA
    if (scop.root.hasAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION))
      options.append(scheduleDomain).append(" -> atomic[xxx]")
    else
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
          l.parallelization.isInnermost = true
        i = -1 // break
      }
      i -= 1
    }

    // add comment (for debugging) and (eventually) declarations outside loop nest
    val comment = IR_Comment("Scop ID: " + scop.ID + ";  Statements: " + scop.stmts.keySet.toArray.sorted.mkString(", "))
    comment +=: nju // prepend
    if (scop.decls.nonEmpty) {
      val scopeList = new ListBuffer[IR_Statement]
      for (decl : IR_VariableDeclaration <- scop.decls) {
        decl.initialValue = None
        if (!scopeList.contains(decl)) // only add if not yet available
          scopeList += decl
      }
      scopeList ++= nju
      IR_Scope(loop.createOMPThreadsWrapper(scopeList))
    } else
      loop.createOMPThreadsWrapper(nju)
  }

  private def processIslNode(node : isl.AstNode) : ListBuffer[IR_Statement] = {

    node.getType match {

      case isl.AstNodeType.NodeFor =>
        if (node.forIsDegenerate()) {
          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val decl : IR_Statement = IR_VariableDeclaration(IR_IntegerDatatype, islIt.getId.getName, IR_ASTExpressionBuilder.processIslExpr(node.forGetInit()))
          processIslNode(node.forGetBody()).+=:(decl)

        } else {
          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val itStr : String = islIt.getId.getName
          val parOMP : Boolean = parallelize_omp && parDims.contains(itStr)
          // TODO: is parallelize_omp still required?
          parallelize_omp &= !parOMP // if code must be parallelized, then now (parNow) XOR later (parallelize)
          val it : IR_VariableAccess = IR_VariableAccess(itStr, IR_IntegerDatatype)
          val init : IR_Statement = IR_VariableDeclaration(IR_IntegerDatatype, itStr, IR_ASTExpressionBuilder.processIslExpr(node.forGetInit()))
          val cond : IR_Expression = IR_ASTExpressionBuilder.processIslExpr(node.forGetCond())
          val incr : IR_Statement = IR_Assignment(it, IR_ASTExpressionBuilder.processIslExpr(node.forGetInc()), "+=")

          val body : ListBuffer[IR_Statement] = processIslNode(node.forGetBody())
          parallelize_omp |= parOMP // restore overall parallelization level
          val loop = new IR_ForLoop(init, cond, incr, body, Duplicate(parallelization))
          loop.parallelization.potentiallyParallel = parDims != null && parDims.contains(itStr)
          loop.parallelization.isVectorizable = vecDims != null && vecDims.contains(itStr)
          loopStmts.getOrElseUpdate(itStr, new ListBuffer()) += loop
          ListBuffer[IR_Statement](loop)
        }

      case isl.AstNodeType.NodeIf =>
        val cond : IR_Expression = IR_ASTExpressionBuilder.processIslExpr(node.ifGetCond())
        val thenBranch : ListBuffer[IR_Statement] = processIslNode(node.ifGetThen())
        if (node.ifHasElse()) {
          val els : ListBuffer[IR_Statement] = processIslNode(node.ifGetElse())
          ListBuffer[IR_Statement](IR_IfCondition(cond, thenBranch, els))
        } else
          ListBuffer[IR_Statement](IR_IfCondition(cond, thenBranch))

      case isl.AstNodeType.NodeBlock =>
        val stmts = new ListBuffer[IR_Statement]
        node.blockGetChildren().foreach({ stmt : isl.AstNode => stmts ++= processIslNode(stmt) })
        stmts

      case isl.AstNodeType.NodeUser =>
        val expr : isl.AstExpr = node.userGetExpr()
        assume(expr.getOpType == isl.AstOpType.OpCall, "user node is no OpCall?!")
        val args : Array[IR_Expression] = IR_ASTExpressionBuilder.processArgs(expr)
        val name : String = args(0).asInstanceOf[IR_StringLiteral].value
        val (oldStmt : ListBuffer[IR_Statement], loopVars : ArrayBuffer[String]) = oldStmts(name)
        val stmts : ListBuffer[IR_Statement] = Duplicate(oldStmt)
        var repl = Map[String, IR_Expression]()
        for (d <- 1 until args.length)
          repl += loopVars(loopVars.size - d) -> args(d)

        IR_ReplaceVariableAccess.replace = repl
        IR_ReplaceVariableAccess.applyStandalone(IR_Scope(stmts))
        for (stmt <- stmts)
          for (cond <- stmt.getAnnotation(IR_PolyOpt.IMPL_CONDITION_ANNOT)) // Option
            IR_ReplaceVariableAccess.applyStandalone(cond.asInstanceOf[IR_Expression])
        stmts

      case isl.AstNodeType.NodeMark  => throw PolyASTBuilderException("unexpected and unknown mark node found...")
      case isl.AstNodeType.NodeError => throw PolyASTBuilderException("NodeError found...")
    }
  }
}

case class PolyASTBuilderException(msg : String = null) extends Exception(msg) {}
