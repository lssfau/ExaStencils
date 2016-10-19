package exastencils.stencil.l2

import exastencils.knowledge.l2.L2_KnowledgeObjectWithLevel
import exastencils.stencil.l3._

/// L2_Operator

abstract class L2_Operator extends L2_KnowledgeObjectWithLevel[L3_Operator]

///// L2_StencilOperator
//
//case class L2_StencilOperator(
//    var name : String, // will be used to find the operator
//    var level : Int, // the level the operator lives on
//    var stencil : L2_Stencil // linked stencil
//) extends L2_Operator[L3_StencilOperator] {
//
//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  def progressImpl() = L3_StencilOperator(name, level, L3_StencilCollection.getByIdentifier(stencil.name, stencil.level).get)
//}
//
///// L2_StencilTemplateOperator
//
//case class L2_StencilTemplateOperator(
//    var name : String, // will be used to find the operator
//    var level : Int, // the level the operator lives on
//    var stencil : L2_StencilTemplate // linked stencil template
//) extends L2_Operator[L3_StencilTemplateOperator] {
//
//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  def progressImpl() = L3_StencilTemplateOperator(name, level, L3_StencilTemplateCollection.getByIdentifier(stencil.name, stencil.level).get)
//}
