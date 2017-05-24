package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.core._
import exastencils.knowledge.l3.L3_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.operator.l4._
import exastencils.optimization.l3._
import exastencils.prettyprinting._
import exastencils.util.l3.L3_ReplaceExpressions

/// L3_Stencil

case class L3_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var numDims : Int, // number of dimensions in the coefficients
    var colStride : Array[Double], // strides of the entries per dimension; 1 means stencil represents a square matrix, >1 means stride*n x n, <1 means n x n/stride
    var entries : ListBuffer[L3_StencilMappingEntry]) extends L3_LeveledKnowledgeObject[L4_Stencil] {

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = L4_Stencil(name, level, entries.map(_.progress))

  def squash() = {
    case class Mapping(var row : L3_ExpressionIndex, var col : L3_ExpressionIndex)

    val newEntries = HashMap[Mapping, L3_Expression]()

    entries.foreach(_.row.indices.transform(L3_SimplifyExpression.simplifyFloatingExpr))
    entries.foreach(_.col.indices.transform(L3_SimplifyExpression.simplifyFloatingExpr))

    for (entry <- entries) {
      val id = Mapping(entry.row, entry.col)
      if (newEntries.contains(id))
        newEntries(id) += entry.coefficient
      else
        newEntries += ((id, entry.coefficient))
    }

    entries = newEntries.to[ListBuffer].sortBy(_._1.col.prettyprint()).map {
      case (mapping, coeff) => L3_StencilMappingEntry(mapping.row, mapping.col, coeff)
    }

    entries.foreach(L3_GeneralSimplify.doUntilDoneStandalone(_))
  }

  def compileCases() : ListBuffer[ListBuffer[Int]] = {
    def numCases(d : Int) : Int = if (colStride(d) >= 1) 1/*colStride(d).toInt*/ else (1.0 / colStride(d)).toInt

    var cases = ListBuffer.range(0, numCases(0)).map(i => ListBuffer(i))
    for (d <- 1 until numDims)
      cases = ListBuffer.range(0, numCases(d)).flatMap(i => cases.map(_ :+ i))

    cases
  }

  def filter() = {
    // remove entries with zero coefficients
    entries = entries.filter(entry => {
      try {
        val simplified = L3_SimplifyExpression.simplifyFloatingExpr(entry.coefficient)
        //entry.coefficient = simplified

        simplified match {
          case L3_RealConstant(0.0) => false
          case _                    => true
        }
      } catch {
        // keep entry if eval is not possible
        case _ : EvaluationException => true
      }
    })

    // remove entries with invalid row/column pairs
    entries = entries.filter(entry => {
      // filter entries with invalid indices
      val cases = compileCases()

      // check if at least one case exists that emits valid indices
      cases.map(curCase => {
        val newIndex = Duplicate(entry.col)
        for (d <- 0 until numDims) {
          L3_ReplaceExpressions.toReplace = entry.row.indices(d)
          L3_ReplaceExpressions.replacement = curCase(d)
          L3_ReplaceExpressions.applyStandalone(newIndex)
        }
        newIndex.indices.map(L3_SimplifyExpression.simplifyFloatingExpr(_) match {
          case L3_RealConstant(v) => v.isValidInt
          case other              => Logger.warn(other); false
        }).reduce(_ && _)
      }).reduce(_ || _)
    })
  }
}
