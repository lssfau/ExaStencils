package exastencils.stencil.l3

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

///// L3_OperatorDeclaration
//
//trait L3_OperatorDeclaration extends L3_Statement {
//  def name : String
//  override def progress = Logger.error(s"Trying to progress L3 operator declaration for operator $name; this is not supported")
//}
//
///// L3_OperatorFromL2
//
//case class L3_OperatorFromL2(var name : String, var level : L3_LevelSpecification = L3_AllLevels) extends L3_OperatorDeclaration {
//  override def prettyprint(out : PpStream) = out << "Operator" << ' ' << name << ' ' << "from" << ' ' << "L2"
//}
//
///// L3_OperatorFromStencil
//
//case class L3_OperatorFromStencil(var name : String, var stencil : L3_StencilDecl) extends L3_OperatorDeclaration {
//  override def prettyprint(out : PpStream) = out << "Operator" << ' ' << name << ' ' << "from" << ' ' << stencil
//}
//
///// L3_ProcessOperatorDeclaration
//
//object L3_ProcessOperatorDeclaration extends DefaultStrategy("Integrate Layer3 operator declarations with knowledge") {
//  this += Transformation("Take over l2 operators", {
//    case operator : L3_OperatorFromL2 =>
//      for (l2Operator <- L2_OperatorCollection.objects)
//        if (l2Operator.name == operator.name)
//          l2Operator match {
//            case op : L2_StencilOperator         => {
//              val L3Stencil = op.stencil.progress
//              L3_StencilCollection.add(L3Stencil)
//              L3_OperatorCollection.add(L3_StencilOperator(operator.name, op.level, L3Stencil))
//            }
//            case op : L2_StencilTemplateOperator => {
//              val L3Stencil = op.stencil.progress
//              L3_StencilTemplateCollection.add(L3Stencil)
//              L3_OperatorCollection.add(L3_StencilTemplateOperator(operator.name, op.level, L3Stencil))
//            }
//            case _                               => Logger.warn(s"Encountered operator of unknown type : $operator")
//          }
//      None // consume declaration statement
//  })
//
//  this += Transformation("Process new operators", {
//    case operator : L3_OperatorFromStencil => {
//      for (op <- L3_StencilCollection.getAllByIdentifier(operator.stencil.name))
//        L3_OperatorCollection.add(L3_StencilOperator(operator.name, op.level, op))
//      None // consume declaration statement
//    }
//  })
//}
