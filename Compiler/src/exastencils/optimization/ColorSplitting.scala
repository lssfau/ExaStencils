package exastencils.optimization

import java.util.IdentityHashMap
import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.util.SimplifyExpression
import exastencils.polyhedron.PolyOpt

object ColorSplitting extends DefaultStrategy("Color Splitting") {

  private val nrColors = 2

  override def apply(node : Option[Node] = None) : Unit = {
    StateManager.register(ColorCondCollector)
    super.apply(node)
    StateManager.unregister(ColorCondCollector)
  }

  this += new Transformation("now", new PartialFunction[Node, Transformation.OutputType] {

    val updatedFields = new IdentityHashMap[Field, Integer]()

    override def isDefinedAt(node : Node) : Boolean = {
      return node.isInstanceOf[DirectFieldAccess]
    }

    private def addColorOffset(index : MultiIndex, dim : Int, colorOffset : Int) : Boolean = {
      val cond : Expression = ColorCondCollector.cond
      if (cond == null)
        return false
      val (expr, cValue) =
        cond match {
          case EqEqExpression(IntegerConstant(c),
            ModuloExpression(sum, IntegerConstant(nrColors2))) if (nrColors == nrColors2) =>
            (sum, c)

          case EqEqExpression(ModuloExpression(sum, IntegerConstant(nrColors2)),
            IntegerConstant(c)) if (nrColors == nrColors2) =>
            (sum, c)

          case _ =>
            return false
        }
      val accCSum = SimplifyExpression.extractIntegralSum(Duplicate(index).reduce((x, y) => x + y))
      val cOffset : Long = accCSum.remove(SimplifyExpression.constName).getOrElse(0L)
      if (accCSum != SimplifyExpression.extractIntegralSum(expr))
        return false
      val color : Long = ((cValue + cOffset) % nrColors + nrColors) % nrColors // mathematical modulo
      index(dim) += IntegerConstant(color * colorOffset)
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
        layout(innerD).total = IntegerConstant(layout(innerD).evalTotal)
        colorOffset = layout(outerD).evalTotal
        layout(outerD).numInnerLayers += colorOffset
        layout(outerD).total = IntegerConstant(layout(outerD).evalTotal)
        updatedFields.put(field, colorOffset)
      }

      val index : MultiIndex = dfa.index
      if (!addColorOffset(index, outerD, colorOffset))
        index(outerD) += (Duplicate(index).reduce((x, y) => x + y) Mod IntegerConstant(nrColors)) * IntegerConstant(colorOffset.longValue())
      index(innerD) = index(innerD) / IntegerConstant(nrColors)

      return dfa
    }
  })
}

object ColorCondCollector extends Collector {

  var cond : Expression = null

  def enter(node : Node) : Unit = {
    node match {
      case loop : LoopOverDimensions =>
        cond = loop.condition.getOrElse(null)
      case _ =>
        val annot : Option[Annotation] = node.getAnnotation(PolyOpt.IMPL_CONDITION_ANNOT)
        if (annot.isDefined)
          cond = annot.get.value.asInstanceOf[Expression]
    }
  }

  def leave(node : Node) : Unit = {
    if (node.isInstanceOf[LoopOverDimensions] || node.hasAnnotation(PolyOpt.IMPL_CONDITION_ANNOT))
      cond = null
  }

  def reset() : Unit = {
    cond = null
  }
}
