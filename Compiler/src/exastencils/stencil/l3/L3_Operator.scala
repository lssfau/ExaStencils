package exastencils.stencil.l3

import exastencils.knowledge.l3._
import exastencils.stencil.l4._

///// L3_StencilOperator
//
//case class L3_StencilOperator(
//    var name : String, // will be used to find the operator
//    var level : Int, // the level the operator lives on
//    var stencil : L3_Stencil // linked stencil
//) extends L3_Operator {
//
//  def progress = L4_StencilOperator(name, level, L4_StencilCollection.getByIdentifier(stencil.identifier, stencil.level).get)
//}
//
///// L3_StencilTemplateOperator
//
//case class L3_StencilTemplateOperator(var identifier : String, // will be used to find the operator
//    var level : Int, // the level the operator lives on
//    var stencil : L3_StencilTemplate // linked stencil template
//) extends L3_Operator {
//
//  def progress = L4_StencilFieldOperator(identifier, level, L4_StencilFieldCollection.getByIdentifier(stencil.name, stencil.level).get)
//}
//
///// L3_GeneralOperator
//
//case class L3_GeneralOperator(// TODO: think about a good name
//    var name : String, // will be used to find the operator
//    var level : Int // the level the operator lives on
//    // TODO: add missing information
//) extends L3_Operator {
//
//  def progress = L4_GeneralOperator(name, level)
//}
