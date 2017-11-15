package exastencils.polyhedron

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.polyhedron.Isl.TypeAliases._

case class IR_GenericTransform(field : String, level : Int, its : Array[IR_VariableAccess], trafo : IR_ExpressionIndex) extends IR_Node {

  def getIslTrafo() : isl.MultiAff = {
    var maff = isl.MultiAff.zero(isl.Space.alloc(Isl.ctx, 0, its.length, trafo.length))
    val lSpace = isl.LocalSpace.fromSpace(maff.getDomainSpace())
    for (i <- 0 until trafo.length)
      try {
        maff = maff.setAff(i, exprToIslAff(trafo(i), lSpace))
      } catch {
        case ExtractionException(msg) =>
          Logger.error("cannot deal with transformation expression " + trafo(i).prettyprint() + ": " + msg)
      }
    maff
  }

  private def exprToIslAff(expr : IR_Expression, lSpace : isl.LocalSpace) : isl.Aff = {
    var aff : isl.Aff = null
    expr match {
      case va : IR_VariableAccess =>
        val itID : Int = its.indexOf(va)
          if (itID < 0)
          throw new ExtractionException("unkown variable access: " + va.prettyprint())
        aff = isl.Aff.varOnDomain(lSpace, T_SET, itID)

      case IR_IntegerConstant(c) =>
        aff = isl.Aff.valOnDomain(lSpace, isl.Val.intFromSi(Isl.ctx, c))

      case IR_Addition(summands : ListBuffer[IR_Expression]) =>
        val it : Iterator[IR_Expression] = summands.iterator
        aff = exprToIslAff(it.next(), lSpace)
        while (it.hasNext && aff != null)
          aff = aff.add(exprToIslAff(it.next(), lSpace))

      case IR_Multiplication(factors : ListBuffer[IR_Expression]) =>
        val it : Iterator[IR_Expression] = factors.iterator
        aff = exprToIslAff(it.next(), lSpace)
        while (it.hasNext && aff != null)
          aff = aff.mul(exprToIslAff(it.next(), lSpace))

      case IR_Subtraction(minu : IR_Expression, subt : IR_Expression) =>
        aff = exprToIslAff(minu, lSpace).sub(exprToIslAff(subt, lSpace))

      case IR_Division(divid : IR_Expression, divis : IR_Expression) =>
        aff = exprToIslAff(divid, lSpace).div(exprToIslAff(divis, lSpace)).floor()

      case IR_Modulo(divid : IR_Expression, divis : IR_Expression) =>
        val divisIsl = exprToIslAff(divis, lSpace)
        if (divisIsl.isCst())
          aff = exprToIslAff(divid, lSpace).modVal(divisIsl.getConstantVal())

      case _ =>
    }

    if (aff == null)
      throw new ExtractionException(expr.prettyprint())
    aff
  }
}
