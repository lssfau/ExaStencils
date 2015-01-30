package exastencils.optimization

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.logger._
import exastencils.util._

object RemoveDupSIMDLoads extends CustomStrategy("Remove duplicate SIMD loads") {

  private[optimization] final val NEW_DECLS_ANNOT = "RDSL_Decls"
  private[optimization] final val REPL_ANNOT = "RDSL_Repl"

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    val annotate = new Analyze()
    StateManager.register(annotate)
    this.execute(new Transformation("analyze", PartialFunction.empty))
    StateManager.unregister(annotate)

    this.execute(new Transformation("adapt", Adapt))

    this.commit()
  }
}

private[optimization] final class Analyze extends Collector {
  import RemoveDupSIMDLoads._

  private var preLoopDecls : ListBuffer[Node] = null
  private var loads : HashMap[(Expression, HashMap[Expression, Long]), VariableDeclarationStatement] = null
  private var upLoopVar : UpdateLoopVar = null

  override def enter(node : Node) : Unit = {
    node match {
      case ForLoopStatement(AssignmentStatement(VariableAccess(lVar, _), start, "="), _,
        AssignmentStatement(VariableAccess(lVar2, _), IntegerConstant(incr), "+="), _, _) if (lVar == lVar2) =>
        if (node.removeAnnotation(Vectorization.VECT_ANNOT).isDefined) {
          preLoopDecls = new ListBuffer[Node]
          node.annotate(NEW_DECLS_ANNOT, preLoopDecls)
          loads = new HashMap[(Expression, HashMap[Expression, Long]), VariableDeclarationStatement]
          upLoopVar = new UpdateLoopVar(lVar, incr, start)
        }

      case decl @ VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp,
        Some(load @ SIMD_LoadExpression(UnaryExpression(UnaryOperators.AddressOf, ArrayAccess(base, index)), aligned))) =>

        val indSum : HashMap[Expression, Long] = SimplifyExpression.extractIntegralSum(index)
        val other = loads.get((base, indSum))

        if (other.isDefined)
          decl.expression = Some(VariableAccess(other.get.name, Some(SIMD_RealDatatype())))

        else {
          loads((base, indSum)) = decl

          // test if the vector can be reused next iteration
          val indSumNIt : HashMap[Expression, Long] = SimplifyExpression.extractIntegralSum(upLoopVar.updateDup(index))
          val nextIt = loads.get((base, indSumNIt))
          if (nextIt.isDefined) {
            preLoopDecls += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp,
              Some(SIMD_LoadExpression(UnaryExpression(UnaryOperators.AddressOf,
                ArrayAccess(Duplicate(base), SimplifyExpression.simplifyIntegralExpr(upLoopVar.replaceDup(index)))), aligned)))
            decl.annotate(REPL_ANNOT, AssignmentStatement(VariableAccess(vecTmp, Some(SIMD_RealDatatype())), load, "="))
            nextIt.get.annotate(REPL_ANNOT, VariableDeclarationStatement(SIMD_RealDatatype(), nextIt.get.name,
              Some(VariableAccess(vecTmp, Some(SIMD_RealDatatype())))))
          }
        }

      case _ => /* nothing to do */
    }
  }

  override def leave(node : Node) : Unit = {
    if (node.hasAnnotation(NEW_DECLS_ANNOT)) {
      preLoopDecls = null
      loads = null
      upLoopVar = null
    }
  }

  override def reset() : Unit = {
    preLoopDecls = null
  }

  private class UpdateLoopVar(name : String, offset : Long, nju : Expression)
      extends DefaultStrategy("Add loop var offset") {

    private final val SKIP_ANNOT = "RDSL_Skip"
    private var replace : Boolean = false

    this += new Transformation("apply", {
      case vAcc @ VariableAccess(v, Some(IntegerDatatype())) if (v == name) =>
        if (replace)
          SubtractionExpression(Duplicate(nju), IntegerConstant(offset))
        else if (!vAcc.removeAnnotation(SKIP_ANNOT).isDefined) {
          vAcc.annotate(SKIP_ANNOT) // already done
          SubtractionExpression(vAcc, IntegerConstant(offset))
        } else
          vAcc
    })

    override def applyStandalone(node : Node) : Unit = {
      val oldLvl = Logger.getLevel
      Logger.setLevel(1)
      super.applyStandalone(node)
      Logger.setLevel(oldLvl)
    }

    def updateDup(expr : Expression) : Expression = {
      val expr2 = FreeStatement(Duplicate(expr)) // just a temporary wrapper...
      replace = false
      applyStandalone(expr2)
      return expr2.pointer
    }

    def replaceDup(expr : Expression) : Expression = {
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
    return node.hasAnnotation(NEW_DECLS_ANNOT) || node.hasAnnotation(REPL_ANNOT)
  }

  def apply(node : Node) : Transformation.OutputType = {

    val decls = node.removeAnnotation(NEW_DECLS_ANNOT)
    if (decls.isDefined) {
      val stmts = decls.get.value.asInstanceOf[ListBuffer[Node]]
      stmts += node
      return stmts
    }

    return node.removeAnnotation(REPL_ANNOT).get.value.asInstanceOf[Node]
  }
}
