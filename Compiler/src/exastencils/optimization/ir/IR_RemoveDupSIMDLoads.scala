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

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.simd._
import exastencils.util.ir.IR_StackCollector

object IR_RemoveDupSIMDLoads extends CustomStrategy("Remove duplicate SIMD loads") {

  private[optimization] final val ADD_BEFORE_ANNOT = "RDSL_AddB"
  private[optimization] final val REPL_ANNOT = "RDSL_Repl"
  private[optimization] final val REMOVE_ANNOT = "RDSL_Rem"

  override def apply() : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    this.execute(new Transformation("sort", SortLoads, isParallel = true))

    val annotate = new Analyze()
    this.register(annotate)
    this.onBefore = () => this.resetCollectors()
    this.execute(new Transformation("analyze", PartialFunction.empty, isParallel = true))
    this.unregister(annotate)

    this.execute(new Transformation("adapt", Adapt, isParallel = true))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }

  private val SortLoads : PartialFunction[Node, Transformation.OutputType] = {
    case l : IR_ForLoop if l.hasAnnotation(IR_Vectorization.VECT_ANNOT) =>
      val newBody = new ListBuffer[IR_Statement]()
      val toSort = new ArrayBuffer[(IR_Statement, IR_Expression, IR_Expression)]()
      for (s <- l.body) {
        s match {
          case IR_VariableDeclaration(SIMD_RealDatatype, _,
          Some(SIMD_Load(IR_AddressOf(IR_ArrayAccess(base, index, _)), _)), _) //
          =>
            toSort += ((s, base, index))

          case IR_VariableDeclaration(SIMD_RealDatatype, _,
          Some(SIMD_Scalar2Vector(IR_ArrayAccess(base, index, _))), _) //
          =>
            toSort += ((s, base, index))

          case IR_VariableDeclaration(SIMD_RealDatatype, _, Some(sh : SIMD_ConcShift), _) =>
            toSort += ((s, sh, null))

          case _ =>
            val sorted = toSort.sorted(new Ordering[(IR_Statement, IR_Expression, IR_Expression)]() {
              def compare(x : (IR_Statement, IR_Expression, IR_Expression), y : (IR_Statement, IR_Expression, IR_Expression)) : Int = {
                (x._2, y._2) match {
                  case (a : SIMD_ConcShift, b : SIMD_ConcShift) => return a.prettyprint() compare b.prettyprint()
                  case (a : SIMD_ConcShift, _)                  => return 1
                  case (_, b : SIMD_ConcShift)                  => return -1
                  case _                                        =>
                }
                val basePpCmp : Int = x._2.prettyprint() compare y._2.prettyprint()
                if (basePpCmp != 0)
                  return basePpCmp
                val indXCst : Long = IR_SimplifyExpression.extractIntegralSum(x._3).getOrElse(IR_SimplifyExpression.constName, 0L)
                val indYCst : Long = IR_SimplifyExpression.extractIntegralSum(y._3).getOrElse(IR_SimplifyExpression.constName, 0L)
                indXCst compare indYCst
              }
            })
            newBody ++= sorted.view.map(_._1)
            toSort.clear()
            newBody += s
        }
      }
      l.body = newBody
      l
  }
}

private[optimization] final class Analyze extends IR_StackCollector {

  import IR_RemoveDupSIMDLoads._

  private var preLoopDecls : ListBuffer[IR_Statement] = null
  private var loads : HashMap[(IR_Expression, HashMap[IR_Expression, Long]), (IR_VariableDeclaration, Buffer[List[Node]])] = null
  private var load1s : HashMap[SIMD_Scalar2Vector, (IR_VariableDeclaration, Buffer[List[Node]])] = null
  private var stores : HashMap[IR_Expression, Buffer[List[Node]]] = null
  private var concShifts : HashMap[SIMD_ConcShift, (IR_VariableDeclaration, Buffer[List[Node]])] = null
  private var replaceAcc : HashMap[String, String] = null
  private var upLoopVar : UpdateLoopVar = null
  private var hasOMPPragma : Boolean = false

  def outSideCondition = !stack.exists(_.isInstanceOf[IR_IfCondition])

  override def enter(node : Node) : Unit = {
    super.enter(node)
    node match {
      case loop @ IR_ForLoop(IR_VariableDeclaration(IR_IntegerDatatype, lVar, Some(start), _),
      IR_Lower(IR_VariableAccess(lVar3, _), end),
      IR_Assignment(IR_VariableAccess(lVar2, _), IR_IntegerConstant(incr), "+="),
      _, _) if lVar == lVar2 && lVar2 == lVar3 //
      =>
        if (node.removeAnnotation(IR_Vectorization.VECT_ANNOT).isDefined) {
          preLoopDecls = new ListBuffer[IR_Statement]
          node.annotate(ADD_BEFORE_ANNOT, preLoopDecls)
          loads = new HashMap[(IR_Expression, HashMap[IR_Expression, Long]), (IR_VariableDeclaration, Buffer[List[Node]])]()
          load1s = new HashMap[SIMD_Scalar2Vector, (IR_VariableDeclaration, Buffer[List[Node]])]()
          stores = new HashMap[IR_Expression, Buffer[List[Node]]]()
          concShifts = new HashMap[SIMD_ConcShift, (IR_VariableDeclaration, Buffer[List[Node]])]()
          replaceAcc = new HashMap[String, String]()
          upLoopVar = new UpdateLoopVar(lVar, incr, start)
          hasOMPPragma = Knowledge.omp_enabled && loop.parallelization.potentiallyParallel && !stack.tail.exists { // caution: stack.head == node // TODO: is there a better way?
            case l : IR_ForLoop => l.parallelization.potentiallyParallel
            case _              => false
          }
        }

      case decl @ IR_VariableDeclaration(SIMD_RealDatatype, vecTmp,
      Some(load @ SIMD_Load(IR_AddressOf(IR_ArrayAccess(base, index, _)), aligned)), _) if outSideCondition =>

        val indSum : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(index)
        val other = loads.get((base, indSum))

        if (other.isDefined) {
          replaceAcc(vecTmp) = other.get._1.name
          decl.annotate(REMOVE_ANNOT)
          other.get._2 += stack // super.stack

        } else {
          loads((base, indSum)) = (decl, ArrayBuffer(stack)) // super.stack

          // test if the vector can be reused next iteration
          if (!hasOMPPragma) {
            val indSumNIt : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(upLoopVar.updateDup(index))
            val nextIt = loads.get((base, indSumNIt))
            if (nextIt.isDefined) {
              preLoopDecls += IR_VariableDeclaration(SIMD_RealDatatype, vecTmp,
                SIMD_Load(IR_AddressOf(
                  IR_ArrayAccess(Duplicate(base), IR_SimplifyExpression.simplifyIntegralExpr(upLoopVar.replaceDup(index)))), aligned))
              decl.annotate(REPL_ANNOT, IR_Assignment(IR_VariableAccess(vecTmp, SIMD_RealDatatype), load, "="))
              if (nextIt.get._1.hasAnnotation(REPL_ANNOT))
                nextIt.get._1.annotate(REPL_ANNOT, IR_Assignment(IR_VariableAccess(nextIt.get._1.name, SIMD_RealDatatype),
                  IR_VariableAccess(vecTmp, SIMD_RealDatatype), "=")) // TODO: check if this is always correct...
              else
                nextIt.get._1.annotate(REPL_ANNOT, IR_VariableDeclaration(SIMD_RealDatatype, nextIt.get._1.name,
                  IR_VariableAccess(vecTmp, SIMD_RealDatatype)))
            }
          }
        }

      case decl @ IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, Some(load : SIMD_Scalar2Vector), _) if load1s != null && outSideCondition =>
        val other = load1s.get(load)
        if (other.isDefined) {
          replaceAcc(vecTmp) = other.get._1.name
          decl.annotate(REMOVE_ANNOT)
          other.get._2 += stack // super.stack
        } else
          load1s(load) = (decl, ArrayBuffer(stack)) // super.stack

      case SIMD_Store(dest, _, _) if outSideCondition =>
        val other = stores.get(dest)
        if (other.isDefined)
          other.get += stack // super.stack
        else
          stores(dest) = ArrayBuffer(stack) // super.stack

      case vAcc @ IR_VariableAccess(vecTmp, SIMD_RealDatatype) if replaceAcc != null && outSideCondition =>
        val nju = replaceAcc.get(vecTmp)
        if (nju.isDefined)
          vAcc.name = nju.get

      case _ => /* nothing to do */
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      // search for duplicate SIMD_ConcShift AFTER VariableAccesses in the subtree are replaced
      case decl @ IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, Some(cShift : SIMD_ConcShift), _) if outSideCondition =>
        val other = concShifts.get(cShift)
        if (other.isDefined) {
          replaceAcc(vecTmp) = other.get._1.name
          decl.annotate(REMOVE_ANNOT)
          other.get._2 += stack // super.stack
        } else
          concShifts(cShift) = (decl, ArrayBuffer(stack)) // super.stack

      case _ => /* nothing to do */
    }

    if (node.hasAnnotation(ADD_BEFORE_ANNOT)) {
      // check if some declarations must be moved out of their scope
      for ((load, ancss) <- loads.values.view ++ load1s.values ++ concShifts.values; if ancss.length > 1) {
        var loadAncs = ancss.head
        for (i <- 1 until ancss.length) {
          val reuseAncs : List[Node] = ancss(i)
          import scala.util.control.Breaks._
          breakable {
            do {
              var rAs : List[Node] = reuseAncs
              val lAParent : Node = loadAncs.tail.head
              do {
                rAs = rAs.tail
                if (lAParent eq rAs.head)
                  break
              } while (!rAs.head.isInstanceOf[IR_ForLoop])
              loadAncs = loadAncs.tail
            } while (true)
          }
        }
        if (load ne loadAncs.head) {
          // load declaration must be moved out, directly before node loadAncs.head
          val annot = loadAncs.head.asInstanceOf[Annotatable]
          annot.annotations.getOrElseUpdate(ADD_BEFORE_ANNOT, new ListBuffer[IR_Statement]()).asInstanceOf[ListBuffer[IR_Statement]] += Duplicate(load)
          load.annotate(REMOVE_ANNOT)
        }
      }
      // test if there are repeated store operations to the same memory location
      for (ancss <- stores.values) {
        import scala.util.control.Breaks._
        for (i <- 0 until ancss.length) breakable {
          val mayRem = ancss(i)
          for (j <- i + 1 until ancss.length) {
            val sameScope = ancss(j)
            if (mayRem.tail eq sameScope.tail) { // do only remove first if there is a second one in the same scope (exactly the same)
              mayRem.head.annotate(REMOVE_ANNOT)
              break // continue i
            }
          }
        }
      }
      preLoopDecls = null
      loads = null
      load1s = null
      stores = null
      concShifts = null
      replaceAcc = null
      upLoopVar = null
    }
    super.leave(node)
  }

  override def reset() : Unit = {
    super.reset()
    preLoopDecls = null
    loads = null
    load1s = null
    concShifts = null
    replaceAcc = null
    upLoopVar = null
  }

  private class UpdateLoopVar(itName : String, offset : Long, nju : IR_Expression)
    extends QuietDefaultStrategy("Add loop var offset") {

    private final val SKIP_ANNOT = "RDSL_Skip"
    private var replace : Boolean = false

    this += new Transformation("apply", {
      case vAcc @ IR_VariableAccess(v, IR_IntegerDatatype) if v == itName =>
        if (replace)
          IR_Subtraction(Duplicate(nju), IR_IntegerConstant(offset))
        else if (vAcc.removeAnnotation(SKIP_ANNOT).isEmpty) {
          vAcc.annotate(SKIP_ANNOT) // already done
          IR_Subtraction(vAcc, IR_IntegerConstant(offset))
        } else
          vAcc
    })

    def updateDup(expr : IR_Expression) : IR_Expression = {
      val expr2 = IR_ArrayFree(Duplicate(expr)) // just a temporary wrapper...
      replace = false
      applyStandalone(expr2)
      expr2.pointer
    }

    def replaceDup(expr : IR_Expression) : IR_Expression = {
      val expr2 = IR_ArrayFree(Duplicate(expr)) // just a temporary wrapper...
      replace = true
      applyStandalone(expr2)
      expr2.pointer
    }
  }

}

private object Adapt extends PartialFunction[Node, Transformation.OutputType] {

  import IR_RemoveDupSIMDLoads._

  def isDefinedAt(node : Node) : Boolean = {
    node.hasAnnotation(ADD_BEFORE_ANNOT) || node.hasAnnotation(REPL_ANNOT) || node.hasAnnotation(REMOVE_ANNOT)
  }

  def apply(node : Node) : Transformation.OutputType = {

    if (node.removeAnnotation(REMOVE_ANNOT).isDefined)
      return None

    val repl = node.removeAnnotation(REPL_ANNOT).asInstanceOf[Option[Node]]
    if (repl.isDefined)
      return repl.get

    val decls = node.removeAnnotation(ADD_BEFORE_ANNOT).asInstanceOf[Option[ListBuffer[IR_Statement]]]
    if (decls.isDefined) {
      decls.get.transform { // some of decls can have a REPL_ANNOT, which must be replaced, but it is not matched (again) in this trafo
        stmt : IR_Statement =>
          val repl = stmt.removeAnnotation(REPL_ANNOT).asInstanceOf[Option[IR_Statement]]
          if (repl.isDefined)
            repl.get
          else
            stmt
      }
      return decls.get += node.asInstanceOf[IR_Statement]
    }

    Logger.error("Adapt.isDefinedAt(Node) does not match Adapt.apply(Node)  in exastencils/optimization/RemoveDupSIMDLoads.scala")
  }
}
