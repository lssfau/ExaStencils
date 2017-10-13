package exastencils.deprecated.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverPointsInOneFragment
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.domain.ir._
import exastencils.field.ir.IR_Field
import exastencils.logger.Logger

/// InitGeomCoords

@deprecated("to be integrated into the new grid class family", "05.10.16")
case class InitGeomCoords(var field : IR_Field, var directCoords : Boolean, var offset : IR_ExpressionIndex = IR_ExpressionIndex(0, 0, 0) /* was float index before */) extends IR_Statement with IR_Expandable {
  override def expand() : Output[StatementList] = {
    if (Knowledge.domain_fragmentTransformation) {
      // TODO: integrate into the new grid class family
      ListBuffer[IR_Statement](
        IR_VariableDeclaration(IR_RealDatatype, "xPosTMP", field.fieldLayout.localization.name.toLowerCase() match {
          case "node" | "face_x"            =>
            Some(((if (directCoords) "x" - field.referenceOffset(0) else "x" : IR_Expression) + offset(0))
              / IR_Cast(IR_RealDatatype, field.fieldLayout.idxById("DRE", 0) - field.fieldLayout.idxById("DLB", 0) - 1)
              * (IR_IV_FragmentPositionEnd(0) - IR_IV_FragmentPositionBegin(0)) + IR_IV_FragmentPositionBegin(0))
          case "cell" | "face_y" | "face_z" =>
            Some(((if (directCoords) "x" - field.referenceOffset(0) else "x" : IR_Expression) + 0.5 + offset(0))
              / IR_Cast(IR_RealDatatype, field.fieldLayout.idxById("DRE", 0) - field.fieldLayout.idxById("DLB", 0) - 0)
              * (IR_IV_FragmentPositionEnd(0) - IR_IV_FragmentPositionBegin(0)) + IR_IV_FragmentPositionBegin(0))
        }),
        IR_VariableDeclaration(IR_RealDatatype, "yPosTMP",
          if (Knowledge.dimensionality > 1) {
            field.fieldLayout.localization.name.toLowerCase() match {
              case "node" | "face_y"            =>
                (((if (directCoords) "y" - field.referenceOffset(1) else "y" : IR_Expression) + offset(1))
                  / IR_Cast(IR_RealDatatype, field.fieldLayout.idxById("DRE", 1) - field.fieldLayout.idxById("DLB", 1) - 1)
                  * (IR_IV_FragmentPositionEnd(1) - IR_IV_FragmentPositionBegin(1)) + IR_IV_FragmentPositionBegin(1))
              case "cell" | "face_x" | "face_z" =>
                (((if (directCoords) "y" - field.referenceOffset(1) else "y" : IR_Expression) + 0.5 + offset(1))
                  / IR_Cast(IR_RealDatatype, field.fieldLayout.idxById("DRE", 1) - field.fieldLayout.idxById("DLB", 1) - 0)
                  * (IR_IV_FragmentPositionEnd(1) - IR_IV_FragmentPositionBegin(1)) + IR_IV_FragmentPositionBegin(1))
            }
          } else IR_IntegerConstant(1)),
        IR_VariableDeclaration(IR_RealDatatype, "zPosTMP",
          if (Knowledge.dimensionality > 2) {
            field.fieldLayout.localization.name.toLowerCase() match {
              case "node" | "face_z"            =>
                (((if (directCoords) "z" - field.referenceOffset(2) else "z" : IR_Expression) + offset(2))
                  / IR_Cast(IR_RealDatatype, field.fieldLayout.idxById("DRE", 2) - field.fieldLayout.idxById("DLB", 2) - 1)
                  * (IR_IV_FragmentPositionEnd(2) - IR_IV_FragmentPositionBegin(2)) + IR_IV_FragmentPositionBegin(2))
              case "cell" | "face_x" | "face_y" =>
                (((if (directCoords) "z" - field.referenceOffset(2) else "z" : IR_Expression) + 0.5 + offset(2))
                  / IR_Cast(IR_RealDatatype, field.fieldLayout.idxById("DRE", 2) - field.fieldLayout.idxById("DLB", 2) - 0)
                  * (IR_IV_FragmentPositionEnd(2) - IR_IV_FragmentPositionBegin(2)) + IR_IV_FragmentPositionBegin(2))
            }
          } else IR_IntegerConstant(1)),
        IR_VariableDeclaration(IR_RealDatatype, "xPos", Some(
          ("xPosTMP" : IR_Expression) * IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 0)
            + ("yPosTMP" : IR_Expression) * IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 1)
            + ("zPosTMP" : IR_Expression) * IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 2)
            + IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 3))),
        if (Knowledge.dimensionality > 1)
          IR_VariableDeclaration(IR_RealDatatype, "yPos", Some(
            ("xPosTMP" : IR_Expression) * IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 4)
              + ("yPosTMP" : IR_Expression) * IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 5)
              + ("zPosTMP" : IR_Expression) * IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 6)
              + IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 7)))
        else IR_NullStatement,
        if (Knowledge.dimensionality > 2)
          IR_VariableDeclaration(IR_RealDatatype, "zPos", Some(
            ("xPosTMP" : IR_Expression) * IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 8)
              + ("yPosTMP" : IR_Expression) * IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 9)
              + ("zPosTMP" : IR_Expression) * IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 10)
              + IR_ArrayAccess(IR_IV_PrimitiveTransformation(), 11)))
        else IR_NullStatement)
    } else {
      Logger.error("deprecated")
    }
  }
}

/// CreateGeomCoordinates

@deprecated("to be integrated into the new grid class family", "05.10.16")
object CreateGeomCoordinates extends DefaultStrategy("Add geometric coordinate calculations") {
  this += new Transformation("Search and extend", {
    case loop : IR_LoopOverPointsInOneFragment =>
      if (StateManager.findFirst[AnyRef]((node : Any) => node match {
        case IR_StringLiteral("xPos") | IR_StringLiteral("yPos") | IR_StringLiteral("zPos")             => true
        case IR_VariableAccess("xPos", _) | IR_VariableAccess("yPos", _) | IR_VariableAccess("zPos", _) => true
        case _                                                                                          => false
      }, loop).isDefined) {
        loop.body.prepend(InitGeomCoords(loop.field, false))
      }

      loop
  })
}
