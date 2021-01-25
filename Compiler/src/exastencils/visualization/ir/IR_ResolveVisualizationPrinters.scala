package exastencils.visualization.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.applications.ns.ir.IR_PrintExodusNNF
import exastencils.applications.ns.ir.IR_PrintExodusNS
import exastencils.applications.ns.ir.IR_PrintVtkNNF
import exastencils.applications.ns.ir.IR_PrintVtkNS
import exastencils.applications.ns.ir.IR_PrintXdmfNNF
import exastencils.applications.ns.ir.IR_PrintXdmfNS
import exastencils.applications.swe.ir.IR_PrintExodusSWE
import exastencils.applications.swe.ir.IR_PrintVtkSWE
import exastencils.applications.swe.ir.IR_PrintXdmfSWE
import exastencils.base.ir.IR_BooleanConstant
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_UnresolvedFunctionReference
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtNode
import exastencils.logger.Logger

/// IR_ResolveVisualizationPrinters

object IR_ResolveVisualizationPrinters extends DefaultStrategy("IR_ResolveVisualizationPrinters") {

  // count number of vis. functions resolved and create an unique "IR_IV_ConstantsWrittenToFile" instance for each
  var funcsResolved : Int = 0
  def getResolveId : Int = {
    val ret = Duplicate(funcsResolved)
    funcsResolved += 1
    ret
  }

  // extract nodal/disc fields from collection of fields passed to the function
  def checkErrors(fields : Seq[IR_Expression]) : Unit = {
    // TODO
    // check for different level specifications in the field access sequence
    val level = fields.collectFirst { case field : IR_FieldAccess => field.level } getOrElse -1 // -1: no field was specified
    fields.foreach {
      case field : IR_FieldAccess if field.level != level =>
        Logger.error("\"IR_ResolveVisualizationPrinters (SWE)\": Field accesses occur on different levels.")
      case _ =>
    }
  }
  def extractNodalFields(fields : Seq[IR_Expression]) : ListBuffer[IR_Field] = {
    checkErrors(fields)

    // collect nodal fields
    fields.to[ListBuffer].collect {
      case acc : IR_FieldAccess if acc.field.localization == IR_AtNode => acc.field
    }.distinct
  }

  def extractDiscFields(fields : Seq[IR_Expression]) : ListBuffer[ListBuffer[IR_Field]] = {
    val discFieldComponents : mutable.HashSet[String] = mutable.HashSet() // no duplicate disc fields
    def throwErrorMsg = Logger.error(s""""IR_ResolveVisualizationPrinters (SWE)": Not enough components specified for disc field (should always consist of 6 cell-centered components).
                                        | The convention is: discFieldLower0, discFieldLower1, discFieldLower2, discFieldUpper0, discFieldUpper1, discFieldUpper2.""".stripMargin)
    checkErrors(fields)

    // begin with cell-centered field and build disc field from the following 6 components
    fields.to[ListBuffer].zipWithIndex.collect {
      case (acc : IR_FieldAccess, index) if acc.field.localization == IR_AtCellCenter && !discFieldComponents.contains(acc.name) =>
        if (index + 6 > fields.length) // not enough fields passed
          throwErrorMsg

        // check if all 6 fields are cell-centered
        val components = fields.slice(index, index + 6).to[ListBuffer] collect {
          case accComponent : IR_FieldAccess if accComponent.field.localization == IR_AtCellCenter =>
            discFieldComponents.add(accComponent.name)
            accComponent.field
        }
        if (components.length != 6)
          throwErrorMsg

        components
    }.distinct
  }

  this += new Transformation("ResolveFunctionCalls", {
    // vtk printers
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printVtkSWE", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), fields @ _*) =>
          IR_PrintVtkSWE(s, i.toInt, getResolveId, extractNodalFields(fields), extractDiscFields(fields))
        case _                                                                 =>
          Logger.error("Malformed call to printVtkSWE; usage: printVtkSWE ( \"filename\", level, , fields : IR_FieldAccess*)")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printVtkNS", _), args))  =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) =>
          IR_PrintVtkNS(s, i.toInt, getResolveId)
        case _                                                    =>
          Logger.error("Malformed call to printVtkNS; usage: printVtkNS ( \"filename\", level )")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printVtkNNF", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) =>
          IR_PrintVtkNNF(s, i.toInt, getResolveId)
        case _                                                    =>
          Logger.error("Malformed call to printVtkNNF; usage: printVtkNNF ( \"filename\", level )")
      }

    // xdmf printers
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printXdmfNNF", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant)                              =>
          IR_PrintXdmfNNF(s, i.toInt, ioInterface, binaryFpp = false, getResolveId)
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant, binFpp : IR_BooleanConstant) =>
          IR_PrintXdmfNNF(s, i.toInt, ioInterface, binFpp.value, getResolveId)
        case _                                                                                                                  =>
          Logger.error("Malformed call to printXdmfNNF; usage: printXdmfNNF ( \"filename\", level, \"ioInterface\", binFpp = false )")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printXdmfNS", _), args))  =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant)                              =>
          IR_PrintXdmfNS(s, i.toInt, ioInterface, binaryFpp = false, getResolveId)
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant, binFpp : IR_BooleanConstant) =>
          IR_PrintXdmfNS(s, i.toInt, ioInterface, binFpp.value, getResolveId)
        case _                                                                                                                  =>
          Logger.error("Malformed call to printXdmfNS; usage: printXdmfNS ( \"filename\", level, \"ioInterface\", binFpp = false )")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printXdmfSWE", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant, fields @ _*)                              =>
          IR_PrintXdmfSWE(s, i.toInt, ioInterface, binaryFpp = false, getResolveId, extractNodalFields(fields), extractDiscFields(fields))
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant, binFpp : IR_BooleanConstant, fields @ _*) =>
          IR_PrintXdmfSWE(s, i.toInt, ioInterface, binFpp.value, getResolveId, extractNodalFields(fields), extractDiscFields(fields))
        case _                                                                                                                               =>
          Logger.error("Malformed call to printXdmfSWE; usage: printXdmfSWE ( \"filename\", level, \"ioInterface\", binFpp = false, fields : IR_FieldAccess*)")
      }

    // exodus printers
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printExodusNNF", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) =>
          IR_PrintExodusNNF(s, i.toInt, getResolveId)
        case _                                                    =>
          Logger.error("Malformed call to printExodusNNF; usage: printExodusNNF ( \"filename\", level )")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printExodusNS", _), args))  =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) =>
          IR_PrintExodusNS(s, i.toInt, getResolveId)
        case _                                                    =>
          Logger.error("Malformed call to printExodusNS; usage: printExodusNS ( \"filename\", level )")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printExodusSWE", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), fields @ _*) =>
          IR_PrintExodusSWE(s, i.toInt, getResolveId, extractNodalFields(fields), extractDiscFields(fields))
        case _                                                                 =>
          Logger.error("Malformed call to printExodusSWE; usage: printExodusSWE ( \"filename\", level, , fields : IR_FieldAccess*)")
      }
  })
}