package exastencils.stencil.l4

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel

//case class L4_StencilOperator(
//    var name : String, // will be used to find the operator
//    var level : Int, // the level the operator lives on
//    var stencil : L4_Stencil // linked stencil
//) extends L4_Operator {
//
//  override def printDecl(out : PpStream) =  stencil.printDecl(out)
//}
//
//case class L4_StencilFieldOperator(
//    var name : String, // will be used to find the operator
//    var level : Int, // the level the operator lives on
//    var stencilField : L4_StencilField // linked stencil field
//) extends L4_Operator {
//
//  override def printDecl(out : PpStream) = {
//    stencilField.stencil.printDecl(out)
//    stencilField.printDecl(out)
//  }
//}
//
//case class L4_GeneralOperator(// TODO: think about a good name
//    var name : String, // will be used to find the operator
//    var level : Int // the level the operator lives on
//    // TODO: add missing information
//) extends L4_Operator {
//
//  override def printDecl(out : PpStream) = ???
//}
