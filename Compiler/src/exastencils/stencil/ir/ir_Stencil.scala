package exastencils.stencil.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.knowledge.ir.IR_KnowledgeObjectWithIdentAndLevel
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.util.ir.IR_ResultingDatatype

/// IR_StencilEntry

case class IR_StencilEntry(var offset : IR_ExpressionIndex, var coefficient : IR_Expression) {
  def datatype : IR_Datatype = coefficient.datatype
}

/// IR_Stencil

object IR_Stencil {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class IR_Stencil(
    var identifier : String,
    var level : Int,
    var entries : ListBuffer[IR_StencilEntry] = new ListBuffer) extends IR_KnowledgeObjectWithIdentAndLevel {

  def datatype = entries.foldLeft(entries.head.datatype)((dt, entry) => IR_ResultingDatatype(dt, entry.datatype))

  def getReach(dim : Int) = entries.map(entry => IR_SimplifyExpression.evalIntegral(entry.offset(dim)).toInt).max

  def findStencilEntry(offset : IR_ExpressionIndex) : Option[IR_StencilEntry] = {
    val index = findStencilEntryIndex(offset)
    if (index.isDefined)
      Some(entries(index.get))
    else
      None
  }

  def findStencilEntryIndex(offset : IR_ExpressionIndex) : Option[Int] = {
    for (i <- entries.indices) {
      val hit = Knowledge.dimensions.map(dim => offset(dim) == entries(i).offset(dim)).reduce(_ & _)
      if (hit)
        return Some(i)
    }

    Logger.warn(s"Trying to find stencil entry for invalid offset ${ offset.prettyprint() } in stencil:\n" +
      entries.map(e => s"\t${ e.offset.prettyprint : String } -> ${ e.coefficient.prettyprint : String }").mkString("\n"))

    None
  }

  def printStencilToStr() : String = {
    var s : String = ""

    s += s"Stencil $identifier:\n\n"

    for (z <- -getReach(2) to getReach(2)) {
      for (y <- -getReach(1) to getReach(1)) {
        for (x <- -getReach(0) to getReach(0))
          s += "\t" +
            entries.find(
              e => e.offset match {
                case index : IR_ExpressionIndex if index.length >= 3 => (
                  (index(0) match { case IR_IntegerConstant(xOff) if x == xOff => true; case _ => false })
                    && (index(1) match { case IR_IntegerConstant(yOff) if y == yOff => true; case _ => false })
                    && (index(2) match { case IR_IntegerConstant(zOff) if z == zOff => true; case _ => false }))
                case _                                               => false
              }).getOrElse(IR_StencilEntry(IR_ExpressionIndex(), 0)).coefficient.prettyprint
        s += "\n"
      }
      s += "\n\n"
    }

    s
  }
}
