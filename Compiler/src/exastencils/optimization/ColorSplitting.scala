package exastencils.optimization

import java.util.IdentityHashMap

import exastencils.base.ir._
import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.polyhedron._
import exastencils.util._

object ColorSplitting extends DefaultStrategy("Color Splitting") {

  private val nrColors = 2
  this.register(ColorCondCollector)

  this += new Transformation("now", new PartialFunction[Node, Transformation.OutputType] {

    val updatedFields = new IdentityHashMap[Field, Integer]()

    override def isDefinedAt(node : Node) : Boolean = {
      return node.isInstanceOf[DirectFieldAccess]
    }

    private def addColorOffset(index : IR_ExpressionIndex, dim : Int, colorOffset : Int) : Boolean = {
      val cond : IR_Expression = ColorCondCollector.cond
      if (cond == null)
        return false
      val (expr, cValue) : (IR_Expression, Long) =
        cond match {
          case IR_EqEqExpression(IR_IntegerConstant(c),
          IR_ModuloExpression(sum, IR_IntegerConstant(nrColors2))) if (nrColors == nrColors2) =>
            (sum, c)

          case IR_EqEqExpression(IR_ModuloExpression(sum, IR_IntegerConstant(nrColors2)),
          IR_IntegerConstant(c)) if (nrColors == nrColors2) =>
            (sum, c)

          case _ =>
            return false
        }
      val accCSum = SimplifyExpression.extractIntegralSum(Duplicate(index).reduce((x, y) => x + y))
      val cOffset : Long = accCSum.remove(SimplifyExpression.constName).getOrElse(0L)
      if (accCSum != SimplifyExpression.extractIntegralSum(expr))
        return false
      val color : Long = ((cValue + cOffset) % nrColors + nrColors) % nrColors // mathematical modulo
      index(dim) += IR_IntegerConstant(color * colorOffset)
      return true
    }

    override def apply(node : Node) : Transformation.OutputType = {

      val dfa = node.asInstanceOf[DirectFieldAccess]
      val field : Field = dfa.fieldSelection.field
      val layout : FieldLayout = field.fieldLayout
      val innerD = 0
      val outerD = layout.layoutsPerDim.length - 1
      var colorOffset : Integer = updatedFields.get(field)
      if (colorOffset == null) {
        layout(innerD).numInnerLayers /= 2
        layout(innerD).numInnerLayers += 1
        layout(innerD).total = layout.idxById("TOT", innerD)
        colorOffset = layout.defIdxById("TOT", outerD)
        layout(outerD).numInnerLayers += colorOffset
        layout(outerD).total = layout.idxById("TOT", outerD)
        updatedFields.put(field, colorOffset)
      }

      val index : IR_ExpressionIndex = dfa.index
      if (!addColorOffset(index, outerD, colorOffset))
        index(outerD) += (Duplicate(index).reduce((x, y) => x + y) Mod IR_IntegerConstant(nrColors)) * IR_IntegerConstant(colorOffset.longValue())
      index(innerD) = index(innerD) / IR_IntegerConstant(nrColors)

      return dfa
    }
  })
}

object ColorCondCollector extends Collector {

  var cond : IR_Expression = null

  override def enter(node : Node) : Unit = {
    node match {
      case loop : LoopOverDimensions if (loop.condition.isDefined && loop.condition.get.isInstanceOf[IR_EqEqExpression]) =>
        cond = loop.condition.get
      case ConditionStatement(c : IR_EqEqExpression, _, fB) if (fB.isEmpty)                                              =>
        cond = c
      case _                                                                                                             =>
        val annot : Option[Any] = node.getAnnotation(PolyOpt.IMPL_CONDITION_ANNOT)
        if (annot.isDefined && annot.get.isInstanceOf[IR_EqEqExpression])
          cond = annot.get.asInstanceOf[IR_Expression]
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case loop : LoopOverDimensions                    => cond = null
      case ConditionStatement(c, _, fB) if (fB.isEmpty) => cond = null
      case _                                            =>
        if (node.hasAnnotation(PolyOpt.IMPL_CONDITION_ANNOT))
          cond = null
    }
  }

  override def reset : Unit = {
    cond = null
  }
}
