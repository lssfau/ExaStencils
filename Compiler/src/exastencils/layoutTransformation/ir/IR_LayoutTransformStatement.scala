package exastencils.layoutTransformation.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldLayoutPerDim
import exastencils.logger.Logger
import exastencils.polyhedron.Isl
import exastencils.polyhedron.Isl.TypeAliases.T_SET

sealed abstract class IR_LayoutTransformStatement extends IR_Statement with IR_SpecialExpandable

case class IR_ExternalFieldAlias(newName : String, field : IR_Field) extends IR_LayoutTransformStatement

case class IR_GenericTransform(field : String, level : Int, its : Array[IR_VariableAccess], trafo : IR_ExpressionIndex) extends IR_LayoutTransformStatement {

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
      throw new ExtractionException(expr.prettyprint() + "; " + expr + " (" + expr.getClass().getSimpleName() + ")")
    aff
  }
}

case class IR_FieldConcatenation(mergedFieldName : String, fieldsToMerge : ListBuffer[IR_Field]) extends IR_LayoutTransformStatement {

  if (fieldsToMerge.size < 2)
    Logger.error("there must be at least to fields to merge")

  val level : Int = fieldsToMerge(0).level
  val slots : Int = fieldsToMerge(0).numSlots

  // check validity and update attributes of newField
  {
    val it = fieldsToMerge.iterator
    val first = it.next()
    val datatype = first.gridDatatype

    while (it.hasNext) {
      val next = it.next()
      if (level != next.level)
        Logger.error("levels of fields to merge do not match!")
      if (slots != next.numSlots)
        Logger.error("number of slots of fields to merge do not match!")
      if (datatype != next.gridDatatype)
        Logger.error("datatypes of fields to merge do not match!")
    }
  }

  def computeNewField() : IR_Field = {
    val newField = Duplicate.forceClone(fieldsToMerge(0))
    newField.name = mergedFieldName
    newField.fieldLayout.name = "merged_" + mergedFieldName

    val dim : Int = newField.fieldLayout.numDimsData + 1
    val nLpD = new Array[IR_FieldLayoutPerDim](dim)
    for (i <- 0 until dim)
      nLpD(i) = IR_FieldLayoutPerDim(0, 0, 0, 0, 0, 0, 0)

    // add new dimensions outermost
    for (f <- fieldsToMerge) {
      val oLpD = f.fieldLayout.layoutsPerDim
      for (i <- 0 until dim - 1) {
        oLpD(i).total match {
          case IR_IntegerConstant(c) =>
            if (c > nLpD(i).numInnerLayers)
              nLpD(i).numInnerLayers = c.toInt
          case _                     =>
            Logger.error(s"size of field $f for dimension $i is not constant")
        }
      }
    }
    nLpD(dim - 1).numInnerLayers = fieldsToMerge.length
    for (i <- 0 until dim)
      nLpD(i).updateTotal()
    newField.fieldLayout.layoutsPerDim = nLpD
    newField
  }
}

private final case class ExtractionException(msg : String) extends Exception(msg)
