package exastencils.fieldlike.l4

import scala.reflect.runtime.universe._
import exastencils.fieldlike.ir.IR_FieldLikeLayout
import exastencils.knowledge.l4.L4_KnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

abstract class L4_FieldLikeLayoutAccess[IR_LayoutLike <: IR_FieldLikeLayout : TypeTag] extends L4_KnowledgeAccess {

  def target : L4_FieldLikeLayout[IR_LayoutLike]

  override def prettyprint(out : PpStream) = out << target.name << "@" << target.level
  override def progress = Logger.error(s"Trying to progress access to field layout ${ target.name }@${ target.level } - unsupported")
}
