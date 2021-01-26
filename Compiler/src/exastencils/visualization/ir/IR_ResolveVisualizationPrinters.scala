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
import exastencils.applications.swe.ir.IR_PrintSionSWE
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

  /* SWE: extract nodal/disc fields from collection of fields passed to the function */
  def checkFieldsAreValid(fieldAccesses : Seq[IR_Expression]) : Unit = {
    val level = fieldAccesses.collectFirst { case acc : IR_FieldAccess => acc.level } getOrElse -1 // -1: no field was specified
    val supportedLocalizations = List(IR_AtNode, IR_AtCellCenter)

    // check if only fields are passed
    if (fieldAccesses.exists { acc => !acc.isInstanceOf[IR_FieldAccess] })
      Logger.error("\"IR_ResolveVisualizationPrinters (SWE)\": Only field accesses should are allowed for parameter: \"fields : IR_FieldAccess*\"")

    // check for correct localizations of fields
    if (fieldAccesses.exists { case acc : IR_FieldAccess => !supportedLocalizations.contains(acc.field.localization) })
      Logger.error("\"IR_ResolveVisualizationPrinters (SWE)\": Only node- or cell-centered fields are allowed.")

    // check if level specification is equal for each field
    if (fieldAccesses.exists { case field : IR_FieldAccess => field.level != level })
      Logger.error("\"IR_ResolveVisualizationPrinters (SWE)\": Field accesses must occur on the same level.")
  }

  // collect nodal fields
  def extractNodalFields(fieldAccesses : Seq[IR_Expression]) : ListBuffer[IR_Field] = {
    checkFieldsAreValid(fieldAccesses)

    fieldAccesses.to[ListBuffer].collect {
      case acc : IR_FieldAccess if acc.field.localization == IR_AtNode => acc.field
    }.distinct
  }

  // collect disc fields
  def extractDiscFields(fieldAccesses : Seq[IR_Expression]) : ListBuffer[ListBuffer[IR_Field]] = {
    val discFieldComponents : mutable.HashSet[String] = mutable.HashSet() // no duplicate disc fields
    def throwErrorMsg = Logger.error(
      s""""IR_ResolveVisualizationPrinters (SWE)": Not enough components specified for disc field (should always consist of 6 cell-centered components).
         | The convention is: discFieldLower0, discFieldLower1, discFieldLower2, discFieldUpper0, discFieldUpper1, discFieldUpper2.""".stripMargin)
    checkFieldsAreValid(fieldAccesses)

    // begin with cell-centered field and build disc field from the following 6 components
    fieldAccesses.to[ListBuffer].zipWithIndex.collect {
      case (acc : IR_FieldAccess, index) if acc.field.localization == IR_AtCellCenter && !discFieldComponents.contains(acc.name) =>
        if (index + 6 > fieldAccesses.length) // not enough fields passed
          throwErrorMsg

        // check if all 6 fields are cell-centered
        val components = fieldAccesses.slice(index, index + 6).to[ListBuffer] collect {
          case accComponent : IR_FieldAccess if accComponent.field.localization == IR_AtCellCenter =>
            discFieldComponents.add(accComponent.name)
            accComponent.field
        }

        // check if disc field has 6 components and if they share a common prefix in their names
        if (components.length != 6)
          throwErrorMsg
        if (components.map(_.name).reduce((a, b) => (a zip b).takeWhile(Function.tupled(_ == _)).map(_._1).mkString).isEmpty)
          Logger.error("\"IR_ResolveVisualizationPrinters (SWE):\" Could not extract a common name from disc field components. Components do not belong to the same disc field.")

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
          Logger.error("Malformed call to printVtkSWE; usage: printVtkSWE ( \"filename\", level, fields : IR_FieldAccess*)")
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
          Logger.error("Malformed call to printExodusSWE; usage: printExodusSWE ( \"filename\", level, fields : IR_FieldAccess*)")
      }

    // sion printer (SWE) for performance comparison
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printSionSWE", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), fields @ _*) =>
          IR_PrintSionSWE(s, i.toInt, getResolveId, extractNodalFields(fields), extractDiscFields(fields))
        case _                                                                 =>
          Logger.error("Malformed call to printSionSWE; usage: printSionSWE ( \"filename\", level, fields : IR_FieldAccess*)")
      }
  })
}