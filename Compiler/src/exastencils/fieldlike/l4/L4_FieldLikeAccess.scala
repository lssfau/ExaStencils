package exastencils.fieldlike.l4

import exastencils.base.l4.L4_CanBeOffset
import exastencils.base.l4.L4_ConstIndex
import exastencils.base.l4.L4_MatIndex
import exastencils.field.l4.L4_SlotSpecification
import exastencils.knowledge.l4.L4_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream

trait L4_FieldLikeAccess extends L4_LeveledKnowledgeAccess with L4_CanBeOffset {
  def target : L4_FieldLike[_, _]
  def slot : L4_SlotSpecification
  def offset : Option[L4_ConstIndex]
  def frozen : Boolean
  def matIndex : Option[L4_MatIndex]

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << target.name
    if (target.numSlots > 1) out << '<' << slot << '>'
    out << '@' << target.level
    if (offset.isDefined) out << "@" << offset.get
    if(matIndex.isDefined) {
      out << matIndex.get
    }
    if (frozen) out << " )"

  }

  def getOffset = offset.getOrElse(L4_ConstIndex(Array.fill(target.numDimsGrid)(0)))
}
