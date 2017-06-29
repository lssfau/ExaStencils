package exastencils.optimization

import scala.collection.mutable

import java.util.IdentityHashMap

import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.polyhedron._

object ColorSplitting extends DefaultStrategy("Color Splitting") {

  private val nrColors = 2
  this.register(ColorCondCollector)

  this += new Transformation("now", new PartialFunction[Node, Transformation.OutputType] {

    val updatedFields = new IdentityHashMap[IR_Field, Integer]()

    override def isDefinedAt(node : Node) : Boolean = {
      node.isInstanceOf[IR_DirectFieldAccess]
    }

    private def addColorOffset(index : IR_ExpressionIndex, dim : Int, colorOffset : Int) : Boolean = {
      val sumMap : mutable.HashMap[IR_Expression, Long] = ColorCondCollector.sum
      val nrCol : Long = ColorCondCollector.nrCol
      val cValue : Long = ColorCondCollector.color
      if (sumMap == null || nrCol != nrColors || cValue < 0 || cValue >= nrColors)
        return false
      val accCSum = IR_SimplifyExpression.extractIntegralSum(Duplicate(index).reduce((x, y) => x + y))
      val cOffset : Long = accCSum.remove(IR_SimplifyExpression.constName).getOrElse(0L)
      if (accCSum != sumMap)
        return false
      val color : Long = ((cValue + cOffset) % nrColors + nrColors) % nrColors // mathematical modulo
      index(dim) += IR_IntegerConstant(color * colorOffset)
      true
    }

    override def apply(node : Node) : Transformation.OutputType = {

      val dfa = node.asInstanceOf[IR_DirectFieldAccess]
      val field : IR_Field = dfa.fieldSelection.field
      val layout : IR_FieldLayout = field.fieldLayout
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

      dfa
    }
  })
}
