package exastencils.fieldlike.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.field.l4.L4_SlotSpecification
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FutureFieldLikeAccess

case class L4_FutureFieldLikeAccess(
    var name : String,
    var level : Int,
    var slot : L4_SlotSpecification,
    var offset : Option[L4_ConstIndex] = None,
    var frozen : Boolean = false,
    var matIndex : Option[L4_MatIndex] = None
) extends L4_FutureKnowledgeAccess with L4_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << name << slot << '@' << level
    if (offset.isDefined) out << '@' << offset
    if(matIndex.isDefined) {
      out << matIndex.get
    }
    if (frozen) out << " )"
  }

  override def progress = Logger.error(s"Trying to progress future field access to $name on level $level")
}

/// L4_ResolveFrozenFields

object L4_ResolveFrozenFields extends DefaultStrategy("Resolve frozen field accesses") {
  this += new Transformation("Resolve", {
    case fct : L4_FunctionCall if "frozen" == fct.name =>
      if (fct.arguments.length != 1) Logger.error("Calls to frozen need exactly one argument")
      if (!fct.arguments.head.isInstanceOf[L4_FutureFieldLikeAccess]) Logger.error("Calls to frozen must done with exactly one field access")
      val fieldAccess = fct.arguments.head.asInstanceOf[L4_FutureFieldLikeAccess]
      fieldAccess.frozen = true
      fieldAccess
  })
}
