package exastencils.stencil.l2

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

///// L2_OperatorDecl
//
//trait L2_OperatorDecl extends L2_Statement {
//  def name : String
//  override def progress = { Logger.error(s"Trying to progress l2 operator declaration for operator $name; this is not supported") }
//}
//
///// L2_OperatorFromStencil
//
//case class L2_OperatorFromStencil(var name : String, var stencil : L2_StencilDecl) extends L2_OperatorDecl {
//  override def prettyprint(out : PpStream) = { out << "Operator" << ' ' << name << ' ' << "from" << ' ' << stencil }
//}
//
///// L2_OperatorFromStencilTemplate
//
//case class L2_OperatorFromStencilTemplate(var name : String, var stencilTemplate : L2_StencilTemplateDecl) extends L2_OperatorDecl {
//  override def prettyprint(out : PpStream) = { out << "Operator" << ' ' << name << ' ' << "from" << ' ' << stencilTemplate }
//}
//
///// L2_ProcessOperatorDeclaration
//
//object L2_ProcessOperatorDeclaration extends DefaultStrategy("Integrate Layer2 operator declarations with knowledge") {
//  this += Transformation("Process new operators", {
//    case operator : L2_OperatorFromStencil         => {
//      for (op <- L2_StencilCollection.getAllByIdentifier(operator.stencil.name))
//        L2_OperatorCollection.add(L2_StencilOperator(operator.name, op.level, op))
//      None // consume declaration statement
//    }
//    case operator : L2_OperatorFromStencilTemplate => {
//      for (op <- L2_StencilTemplateCollection.getAllByIdentifier(operator.stencilTemplate.name))
//        L2_OperatorCollection.add(L2_StencilTemplateOperator(operator.name, op.level, op))
//      None // consume declaration statement
//    }
//  })
//}
