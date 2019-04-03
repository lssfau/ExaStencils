package exastencils.layoutTransformation.ir

import scala.collection.mutable.{ ArrayBuffer, HashMap, ListBuffer }

import java.util.IdentityHashMap

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.polyhedron.Isl
import exastencils.polyhedron.Isl.TypeAliases.T_SET

sealed abstract class IR_LayoutTransformStatement extends IR_Statement with IR_SpecialExpandable

case class IR_ExternalFieldAlias(newName : String, oldName : String, oldLevels : Seq[Int]) extends IR_LayoutTransformStatement

case class IR_GenericTransform(fields : Seq[(String, Int)], its : Array[IR_VariableAccess], trafo : IR_ExpressionIndex) extends IR_LayoutTransformStatement {

  def inDim : Int = its.length
  def outDim : Int = trafo.length

  def getIslTrafo() : isl.MultiAff = {
    var maff = isl.MultiAff.zero(isl.Space.alloc(Isl.ctx, 0, inDim, outDim))
    val lSpace = isl.LocalSpace.fromSpace(maff.getDomainSpace())
    for (i <- 0 until outDim)
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

case class IR_FieldConcatenation(mergedFieldName : String, fieldsToMerge : Seq[String], levels : Seq[Int]) extends IR_LayoutTransformStatement {

  if (fieldsToMerge.size < 2)
    Logger.error(s"there must be at least two fields to merge (for $mergedFieldName)")

  def addFieldReplacements(replace : IdentityHashMap[IR_Field, (IR_Field, Int)]) : Unit = {

    val newFields = new Array[IR_Field](Knowledge.maxLevel + 1)
    val idMap = new HashMap[(String, Int), Int]()
    for ((name, ind) <- fieldsToMerge.view.zipWithIndex)
      for (lvl <- levels)
        idMap((name, lvl)) = ind
    val toRemove = ArrayBuffer[IR_Field]()

    for (field <- IR_FieldCollection.objects)
      for (id <- idMap.remove((field.name, field.level))) {

        if (field.numSlots != 1)
          Logger.error("concat slotted fields is not yet supported! (reason: all of their advance statements will affect the merged field, too, which results in too many advances)")

        val dim : Int = field.fieldLayout.numDimsData + 1

        // create new field, if it does not exist yet
        if (newFields(field.level) == null) {
          val newField = field.createDuplicate()
          newField.name = mergedFieldName
          newField.fieldLayout = field.fieldLayout.createDuplicate()
          newField.fieldLayout.name = "merged_" + mergedFieldName
          newField.fieldLayout.layoutsPerDim = Array.fill(dim)(IR_FieldLayoutPerDim(0, 0, 0, 0, 0, 0, 0))
          newField.fieldLayout.layoutsPerDim(dim - 1).numInnerLayers = fieldsToMerge.length
          newField.fieldLayout.datatype = IR_ArrayDatatype(newField.fieldLayout.datatype, fieldsToMerge.length)
          newFields(field.level) = newField
        }

        val newField : IR_Field = newFields(field.level)

        // some validity checks
        if (newField.numSlots != field.numSlots)
          Logger.error(s"slots of fields to merge for '$mergedFieldName' do not match!")
        if (newField.gridDatatype.resolveBaseDatatype != field.gridDatatype.resolveBaseDatatype)
          Logger.error(s"base datatypes of fields to merge for '$mergedFieldName' do not match!")
        if (newField.fieldLayout.numDimsData != dim)
          Logger.error(s"dimensionalities of fields to merge for '$mergedFieldName' do not match!")

        // update layout
        val oLpD = field.fieldLayout.layoutsPerDim
        val nLpD = newField.fieldLayout.layoutsPerDim
        for (i <- 0 until dim - 1) {
          oLpD(i).total match {
            case IR_IntegerConstant(c) =>
              if (c > nLpD(i).numInnerLayers)
                nLpD(i).numInnerLayers = c.toInt
            case _                     =>
              Logger.error(s"size of field ${ field.name } for dimension $i is not constant")
          }
        }

        replace.put(field, (newField, id))
        toRemove += field
      }
    if (!idMap.isEmpty)
      Logger.error("merge failed, did not find fields for: " + idMap.keySet)

    // update all total values and register new fields to IR_FieldCollection
    for (field <- newFields)
      if (field != null) {
        for (lpd <- field.fieldLayout.layoutsPerDim)
          lpd.updateTotal()
        IR_FieldCollection.add(field)
      }

    // remove all fields that will be replaced from IR_FieldCollection
    for (oldField <- toRemove)
      IR_FieldCollection.remove(oldField)
  }
}

private final case class ExtractionException(msg : String) extends Exception(msg)
