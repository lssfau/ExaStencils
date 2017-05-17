package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ExpressionIndex
import exastencils.baseExt.l2.L2_FieldIteratorAccess
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.operator.l3._
import exastencils.optimization.l2.L2_GeneralSimplify
import exastencils.prettyprinting._

/// L2_Stencil

case class L2_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var numDims : Int, // number of dimensions in the coefficients
    var colStride : Array[Double], // strides of the entries per dimension; 1 means stencil represents a square matrix, >1 means stride*n x n, <1 means n x n/stride
    var entries : ListBuffer[L2_StencilMappingEntry]) extends L2_LeveledKnowledgeObject[L3_Stencil] {

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = L3_Stencil(name, level, entries.map(_.progress))

  def kron(other : L2_Stencil) : L2_Stencil = {
    val otherCloned = Duplicate(other)

    if (level != other.level)
      Logger.warn(s"Level mismatch: $level vs ${ other.level }")

    object ShiftIteratorAccess extends DefaultStrategy("Replace something with something else") {
      var baseDim : Int = 0

      this += new Transformation("Search and replace", {
        case it : L2_FieldIteratorAccess =>
          if (it.dim < baseDim) it.dim += baseDim
          it
      }, false)
    }

    ShiftIteratorAccess.baseDim = numDims
    ShiftIteratorAccess.applyStandalone(entries)

    val newStencil = L2_Stencil(name + "_kron_" + otherCloned.name,
      level,
      numDims + otherCloned.numDims,
      Duplicate(colStride ++ otherCloned.colStride),
      entries.flatMap(l => otherCloned.entries.map(r =>
        Duplicate(L2_StencilMappingEntry(
          L2_ExpressionIndex(l.row.indices ++ r.row.indices),
          L2_ExpressionIndex(l.col.indices ++ r.col.indices),
          l.coefficient * r.coefficient)))))

    newStencil.entries.foreach(e => L2_GeneralSimplify.doUntilDoneStandalone(e))

    newStencil
  }

}
