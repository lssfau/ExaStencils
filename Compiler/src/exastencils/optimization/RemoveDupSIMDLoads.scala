package exastencils.optimization

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.ir._
import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.logger._
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.util._

object RemoveDupSIMDLoads extends CustomStrategy("Remove duplicate SIMD loads") {

  private[optimization] final val ADD_BEFORE_ANNOT = "RDSL_AddB"
  private[optimization] final val REPL_ANNOT = "RDSL_Repl"
  private[optimization] final val REMOVE_ANNOT = "RDSL_Rem"

  override def apply() : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    this.execute(new Transformation("sort", SortLoads))

    val annotate = new Analyze()
    this.register(annotate)
    this.execute(new Transformation("analyze", PartialFunction.empty))
    this.unregister(annotate)

    this.execute(new Transformation("adapt", Adapt))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }

  private val SortLoads : PartialFunction[Node, Transformation.OutputType] = {
    case l : ForLoopStatement if (l.hasAnnotation(Vectorization.VECT_ANNOT)) =>
      val newBody = new ListBuffer[Statement]()
      val toSort = new ArrayBuffer[(Statement, IR_Expression, IR_Expression)]()
      for (s <- l.body) {
        s match {
          case VariableDeclarationStatement(SIMD_RealDatatype, _,
          Some(SIMD_LoadExpression(IR_AddressofExpression(ArrayAccess(base, index, _)), _))) //
          =>
            toSort += ((s, base, index))

          case VariableDeclarationStatement(SIMD_RealDatatype, _,
          Some(SIMD_Scalar2VectorExpression(ArrayAccess(base, index, _)))) //
          =>
            toSort += ((s, base, index))

          case VariableDeclarationStatement(SIMD_RealDatatype, _, Some(sh : SIMD_ConcShift)) =>
            toSort += ((s, sh, null))

          case _ =>
            val sorted = toSort.sorted(new Ordering[(Statement, IR_Expression, IR_Expression)]() {
              def compare(x : (Statement, IR_Expression, IR_Expression), y : (Statement, IR_Expression, IR_Expression)) : Int = {
                (x._2, y._2) match {
                  case (a : SIMD_ConcShift, b : SIMD_ConcShift) => return a.prettyprint() compare b.prettyprint()
                  case (a : SIMD_ConcShift, _)                  => return 1
                  case (_, b : SIMD_ConcShift)                  => return -1
                  case _                                        =>
                }
                val basePpCmp : Int = x._2.prettyprint() compare y._2.prettyprint()
                if (basePpCmp != 0)
                  return basePpCmp
                val indXCst : Long = SimplifyExpression.extractIntegralSum(x._3).getOrElse(SimplifyExpression.constName, 0L)
                val indYCst : Long = SimplifyExpression.extractIntegralSum(y._3).getOrElse(SimplifyExpression.constName, 0L)
                return indXCst compare indYCst
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

private[optimization] final class Analyze extends StackCollector {

  import RemoveDupSIMDLoads._

  private var preLoopDecls : ListBuffer[Statement] = null
  private var loads : HashMap[(IR_Expression, HashMap[IR_Expression, Long]), (VariableDeclarationStatement, Buffer[List[Node]])] = null
  private var load1s : HashMap[SIMD_Scalar2VectorExpression, (VariableDeclarationStatement, Buffer[List[Node]])] = null
  private var concShifts : HashMap[SIMD_ConcShift, (VariableDeclarationStatement, Buffer[List[Node]])] = null
  private var replaceAcc : HashMap[String, String] = null
  private var upLoopVar : UpdateLoopVar = null
  private var hasOMPPragma : Boolean = false

  override def enter(node : Node) : Unit = {
    super.enter(node)
    node match {
      case ForLoopStatement(VariableDeclarationStatement(IR_IntegerDatatype, lVar, Some(start)),
      IR_LowerExpression(VariableAccess(lVar3, _), end),
      AssignmentStatement(VariableAccess(lVar2, _), IntegerConstant(incr), "+="),
      _, _) if (lVar == lVar2 && lVar2 == lVar3) //
      =>
        if (node.removeAnnotation(Vectorization.VECT_ANNOT).isDefined) {
          preLoopDecls = new ListBuffer[Statement]
          node.annotate(ADD_BEFORE_ANNOT, preLoopDecls)
          loads = new HashMap[(IR_Expression, HashMap[IR_Expression, Long]), (VariableDeclarationStatement, Buffer[List[Node]])]()
          load1s = new HashMap[SIMD_Scalar2VectorExpression, (VariableDeclarationStatement, Buffer[List[Node]])]()
          concShifts = new HashMap[SIMD_ConcShift, (VariableDeclarationStatement, Buffer[List[Node]])]()
          replaceAcc = new HashMap[String, String]()
          upLoopVar = new UpdateLoopVar(lVar, incr, start)
          hasOMPPragma = node.isInstanceOf[OMP_PotentiallyParallel]
        }

      case decl @ VariableDeclarationStatement(SIMD_RealDatatype, vecTmp,
      Some(load @ SIMD_LoadExpression(IR_AddressofExpression(ArrayAccess(base, index, _)), aligned))) =>

        val indSum : HashMap[IR_Expression, Long] = SimplifyExpression.extractIntegralSum(index)
        val other = loads.get((base, indSum))

        if (other.isDefined) {
          replaceAcc(vecTmp) = other.get._1.name
          decl.annotate(REMOVE_ANNOT)
          other.get._2 += stack.elems // super.stack

        } else {
          loads((base, indSum)) = (decl, ArrayBuffer(stack.elems)) // super.stack

          // test if the vector can be reused next iteration
          if (!hasOMPPragma) {
            val indSumNIt : HashMap[IR_Expression, Long] = SimplifyExpression.extractIntegralSum(upLoopVar.updateDup(index))
            val nextIt = loads.get((base, indSumNIt))
            if (nextIt.isDefined) {
              preLoopDecls += new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp,
                SIMD_LoadExpression(IR_AddressofExpression(
                  ArrayAccess(Duplicate(base), SimplifyExpression.simplifyIntegralExpr(upLoopVar.replaceDup(index)))), aligned))
              decl.annotate(REPL_ANNOT, AssignmentStatement(new VariableAccess(vecTmp, SIMD_RealDatatype), load, "="))
              if (nextIt.get._1.hasAnnotation(REPL_ANNOT))
                nextIt.get._1.annotate(REPL_ANNOT, AssignmentStatement(new VariableAccess(nextIt.get._1.name, SIMD_RealDatatype),
                  new VariableAccess(vecTmp, SIMD_RealDatatype), "=")) // TODO: check if this is always correct...
              else
                nextIt.get._1.annotate(REPL_ANNOT, new VariableDeclarationStatement(SIMD_RealDatatype, nextIt.get._1.name,
                  new VariableAccess(vecTmp, SIMD_RealDatatype)))
            }
          }
        }

      case decl @ VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, Some(load : SIMD_Scalar2VectorExpression)) if (load1s != null) =>
        val other = load1s.get(load)
        if (other.isDefined) {
          replaceAcc(vecTmp) = other.get._1.name
          decl.annotate(REMOVE_ANNOT)
          other.get._2 += stack.elems // super.stack
        } else
          load1s(load) = (decl, ArrayBuffer(stack.elems)) // super.stack

      case vAcc @ VariableAccess(vecTmp, Some(SIMD_RealDatatype)) if (replaceAcc != null) =>
        val nju = replaceAcc.get(vecTmp)
        if (nju.isDefined)
          vAcc.name = nju.get

      case _ => /* nothing to do */
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      // search for duplicate SIMD_ConcShift AFTER VariableAccesses in the subtree are replaced
      case decl @ VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, Some(cShift : SIMD_ConcShift)) =>
        val other = concShifts.get(cShift)
        if (other.isDefined) {
          replaceAcc(vecTmp) = other.get._1.name
          decl.annotate(REMOVE_ANNOT)
          other.get._2 += stack.elems // super.stack
        } else
          concShifts(cShift) = (decl, ArrayBuffer(stack.elems)) // super.stack

      case _ => /* nothing to do */
    }

    if (node.hasAnnotation(ADD_BEFORE_ANNOT)) {
      // check if some declarations must be moved out of their scope
      for ((load, ancss) <- loads.values.view ++ load1s.values ++ concShifts.values; if (ancss.length > 1)) {
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
              } while (!rAs.head.isInstanceOf[ForLoopStatement])
              loadAncs = loadAncs.tail
            } while (true)
          }
        }
        if (load ne loadAncs.head) {
          // load declaration must be moved out, directly before node loadAncs.head
          val annot = loadAncs.head.asInstanceOf[Annotatable]
          annot.annotations.getOrElseUpdate(ADD_BEFORE_ANNOT, new ListBuffer[Statement]()).asInstanceOf[ListBuffer[Statement]] += Duplicate(load)
          load.annotate(REMOVE_ANNOT)
        }
      }
      preLoopDecls = null
      loads = null
      load1s = null
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
      case vAcc @ VariableAccess(v, Some(IR_IntegerDatatype)) if (v == itName) =>
        if (replace)
          IR_SubtractionExpression(Duplicate(nju), IntegerConstant(offset))
        else if (!vAcc.removeAnnotation(SKIP_ANNOT).isDefined) {
          vAcc.annotate(SKIP_ANNOT) // already done
          IR_SubtractionExpression(vAcc, IntegerConstant(offset))
        } else
          vAcc
    })

    def updateDup(expr : IR_Expression) : IR_Expression = {
      val expr2 = FreeStatement(Duplicate(expr)) // just a temporary wrapper...
      replace = false
      applyStandalone(expr2)
      return expr2.pointer
    }

    def replaceDup(expr : IR_Expression) : IR_Expression = {
      val expr2 = FreeStatement(Duplicate(expr)) // just a temporary wrapper...
      replace = true
      applyStandalone(expr2)
      return expr2.pointer
    }
  }

}

private final object Adapt extends PartialFunction[Node, Transformation.OutputType] {

  import RemoveDupSIMDLoads._

  def isDefinedAt(node : Node) : Boolean = {
    return node.hasAnnotation(ADD_BEFORE_ANNOT) || node.hasAnnotation(REPL_ANNOT) || node.hasAnnotation(REMOVE_ANNOT)
  }

  def apply(node : Node) : Transformation.OutputType = {

    if (node.removeAnnotation(REMOVE_ANNOT).isDefined)
      return None

    val repl = node.removeAnnotation(REPL_ANNOT).asInstanceOf[Option[Node]]
    if (repl.isDefined)
      return repl.get

    val decls = node.removeAnnotation(ADD_BEFORE_ANNOT).asInstanceOf[Option[ListBuffer[Statement]]]
    if (decls.isDefined) {
      decls.get.transform { // some of decls can have a REPL_ANNOT, which must be replaced, but it is not matched (again) in this trafo
        stmt : Statement =>
          val repl = stmt.removeAnnotation(REPL_ANNOT).asInstanceOf[Option[Statement]]
          if (repl.isDefined)
            repl.get
          else
            stmt
      }
      return decls.get += node.asInstanceOf[Statement]
    }

    Logger.error("Adapt.isDefinedAt(Node) does not match Adapt.apply(Node)  in exastencils/optimization/RemoveDupSIMDLoads.scala")
  }
}
