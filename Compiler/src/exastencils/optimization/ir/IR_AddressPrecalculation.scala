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

package exastencils.optimization.ir

import scala.collection.immutable.StringLike
import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_InternalVariableLike
import exastencils.config.Settings
import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.logger._

object IR_AddressPrecalculation extends CustomStrategy("Perform address precalculation") {

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
    this.execute(new Transformation("Find relevant loops and accesses", PartialFunction.empty, isParallel = true))
    this.unregister(annotate)

    this.execute(new Transformation("Optimize", IntegrateAnnotations, isParallel = true))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }
}

private final class ArrayBases(val arrayName : String, val arrayDType : IR_Datatype) {

  private val inits = new HashMap[HashMap[IR_Expression, Long], (String, IR_Expression)]()
  private var idCount = -1

  def getName(initVec : HashMap[IR_Expression, Long], base : IR_Expression, al : Boolean) : String = {
    inits.getOrElseUpdate(initVec, { idCount += 1; (arrayName + "_p" + idCount, IR_ArrayAccess(base, IR_SimplifyExpression.recreateExprFromIntSum(initVec), al)) })._1
  }

  def addToDecls(decls : ListBuffer[IR_Statement]) : Unit = {
    for ((name : String, init : IR_Expression) <- inits.values.toArray.sortBy(_._1))
      decls += IR_VariableDeclaration(arrayDType, name, IR_AddressOf(init))
  }
}

private final class AnnotateLoopsAndAccesses extends Collector {

  import IR_AddressPrecalculation._

  private def generateName(expr : IR_Expression) : String = {
    filter('_' + expr.prettyprint())
  }

  private def filter(cpp : StringLike[_]) : String = {
    val res = new StringBuilder()
    for (c : Char <- cpp)
      c match {
        case '[' | '.' | '%' | '+' | '-' => res.append('_')
        case ']' | '(' | ')' | ' '       =>
        case _                           => res.append(c)
      }
    res.toString()
  }

  def containsRestrictedVar(expr : IR_Expression, allowed : String = null) : Boolean = {
    object Search extends QuietDefaultStrategy("Anonymous search") {
      var res : Boolean = false
      var allowed : String = null
      this += new Transformation("contains loop var", {
        case strC : IR_StringLiteral     =>
          val name = strC.value
          res |= chVars.contains(name) && (allowed != name)
          res |= inVars.contains(name)
          strC
        case varA : IR_VariableAccess    =>
          val name = varA.name
          res |= chVars.contains(name) && (allowed != name)
          res |= inVars.contains(name)
          varA
        case i : IR_InternalVariableLike =>
          val name = i.resolveName()
          res |= chVars.contains(name) && (allowed != name)
          res |= inVars.contains(name)
          i
      })
    }
    Search.res = false
    Search.allowed = allowed
    Search.applyStandalone(IR_Return(expr)) // wrap to ensure ALL nodes of expr are visited
    Search.res
  }

  private def splitIndex(ind : IR_Expression) : (IR_Expression, HashMap[IR_Expression, Long]) = {

    val outMap = new HashMap[IR_Expression, Long]
    if (inVars == null)
      return (ind, outMap)

    // TODO: add support for MultiIndexExpression?
    val inMap : HashMap[IR_Expression, Long] =
      try {
        IR_SimplifyExpression.extractIntegralSum(ind)
      } catch {
        case ex : EvaluationException =>
          var cause : Throwable = ex
          while (cause.getCause != null)
            cause = cause.getCause
          val stackTraceHead = cause.getStackTrace()(0)
          Logger.dbg("[APC]  cannot deal with index expression  (" + ex.msg + ")  in  " + ind.prettyprint() +
            "  (" + stackTraceHead.getFileName + ':' + stackTraceHead.getLineNumber + ')')
          return (ind, outMap)
      }

    // constant part should stay inside the loop, as this reduces the number of required pointers outside
    for ((expr, value) <- inMap)
      if (expr != IR_SimplifyExpression.constName && !containsRestrictedVar(expr))
        outMap.put(expr, value)
    for ((expr, _) <- outMap)
      inMap.remove(expr)

    (IR_SimplifyExpression.recreateExprFromIntSum(inMap), outMap)
  }

  private val SKIP_SUBTREE_ANNOT = "APCSST"
  private var skipSubtree : Boolean = false

  private var decls : HashMap[String, ArrayBases] = null
  private var inVars : Set[String] = null // these variables must stay inside the loop: NO address precalc for them
  private val chVars : Set[String] = Set() // elements of these arrays get modified, so if an access to these appears inside another access, dot perform a precalc!
  private val toAnalyze = new ListBuffer[IR_ArrayAccess]()

  private def isInInner() : Boolean = decls != null
  private def isValid() : Boolean = inVars != null

  override def enter(node : Node) : Unit = {

    if (skipSubtree)
      return

    node match {
      case l : IR_ForLoop if l.parallelization.isInnermost =>
        if (isInInner()) {
          Logger.dbg("ups, nested \"innermost\" loops... something is wrong here")
          decls = null
          return
        }
        val d = new HashMap[String, ArrayBases]()
        l.inc match { // TODO: remove StringLiteral
          case IR_Assignment(IR_VariableAccess(nName, _), _, _)                      =>
            decls = d
            inVars = Set(nName)
          case IR_Assignment(IR_StringLiteral(nName), _, _)                          =>
            decls = d
            inVars = Set(nName)
          case IR_ExpressionStatement(IR_PreIncrement(IR_VariableAccess(nName, _)))  =>
            decls = d
            inVars = Set(nName)
          case IR_ExpressionStatement(IR_PreIncrement(IR_StringLiteral(nName)))      =>
            decls = d
            inVars = Set(nName)
          case IR_ExpressionStatement(IR_PostIncrement(IR_VariableAccess(nName, _))) =>
            decls = d
            inVars = Set(nName)
          case IR_ExpressionStatement(IR_PostIncrement(IR_StringLiteral(nName)))     =>
            decls = d
            inVars = Set(nName)
          case IR_ExpressionStatement(IR_PreDecrement(IR_VariableAccess(nName, _)))  =>
            decls = d
            inVars = Set(nName)
          case IR_ExpressionStatement(IR_PreDecrement(IR_StringLiteral(nName)))      =>
            decls = d
            inVars = Set(nName)
          case IR_ExpressionStatement(IR_PostDecrement(IR_VariableAccess(nName, _))) =>
            decls = d
            inVars = Set(nName)
          case IR_ExpressionStatement(IR_PostDecrement(IR_StringLiteral(nName)))     =>
            decls = d
            inVars = Set(nName)
          case _                                                                     =>
            Logger.dbg("[addr precalc]  cannot determine loop variable name, inc of ForLoopStatement is not recognized:  " + l.inc)
            decls = d
        }
        node.annotate(DECLS_ANNOT, d)

      // ArrayAccess with a constant index only cannot be optimized further
      case acc : IR_ArrayAccess if isInInner() && !acc.index.isInstanceOf[IR_IntegerConstant] =>
        acc.annotate(SKIP_SUBTREE_ANNOT) // skip other ArrayAccesses below this one
        skipSubtree = true
        toAnalyze += acc

      case IR_Assignment(dst, _, _) if isInInner() && isValid() =>
        dst match {
          case _ : IR_StringLiteral
               | _ : IR_VariableAccess
               | _ : IR_ArrayAccess
               | _ : IR_InternalVariableLike =>
            val (name, isArrayAcc) = resolveName(dst)
            if (isArrayAcc)
              chVars += name
            else
              inVars += name
          case _                             => // nothing; expand match here, if more vars should stay inside the loop
        }

      case IR_VariableDeclaration(_, nName, _, _) if isInInner() && isValid() =>
        inVars += nName

      case _ => // ignore
    }
  }

  override def leave(node : Node) : Unit = {

    if (node.removeAnnotation(SKIP_SUBTREE_ANNOT).isDefined)
      skipSubtree = false

    node match {
      case l : IR_ForLoop if l.parallelization.isInnermost =>
        // if base is ArrayAccess we ensure that it does not contain anything, which is written in the loop
        //   (the name of this access itself is not critical, see IR_Assignment match in enter(..))
        for (acc @ IR_ArrayAccess(base, index, al) <- toAnalyze) if (!containsRestrictedVar(base, resolveName(base)._1)) {
          val (in : IR_Expression, outMap : HashMap[IR_Expression, Long]) = splitIndex(index)
          var name : String = generateName(base)
          val datatype = IR_ConstPointerDatatype(base.datatype.resolveBaseDatatype)
          val bases : ArrayBases = decls.getOrElseUpdate(name, new ArrayBases(name, datatype))
          name = bases.getName(outMap, base, al)
          val newAcc = IR_ArrayAccess(IR_VariableAccess(name, datatype), in, al)
          newAcc.annotate(ORIG_IND_ANNOT, Duplicate(index)) // save old (complete) index expression for vectorization
          acc.annotate(REPL_ANNOT, newAcc)
        }
        decls = null
        inVars = null
        chVars.clear()
        toAnalyze.clear()
      case _                                               => // ignore
    }
  }

  override def reset() : Unit = {
    skipSubtree = false
    decls = null
    inVars = null
    chVars.clear()
    toAnalyze.clear()
  }

  // returns identifier and a flag, which indicates if it is an array access
  private def resolveName(expr : IR_Expression) : (String, Boolean) = {
    expr match {
      case IR_ArrayAccess(base, _, _)  => (resolveName(base)._1, true)
      case IR_VariableAccess(nam, _)   => (nam, false)
      case IR_StringLiteral(str)       => (str, false)
      case i : IR_InternalVariableLike => (i.resolveName(), i.resolveAccess(null, null, null, null, IR_IntegerConstant(0), null).isInstanceOf[IR_ArrayAccess])
    }
  }
}

private object IntegrateAnnotations extends PartialFunction[Node, Transformation.OutputType] {

  import IR_AddressPrecalculation._

  def isDefinedAt(node : Node) : Boolean = {
    node.hasAnnotation(DECLS_ANNOT) || node.hasAnnotation(REPL_ANNOT)
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
    IR_Scope(stmts)
  }
}
