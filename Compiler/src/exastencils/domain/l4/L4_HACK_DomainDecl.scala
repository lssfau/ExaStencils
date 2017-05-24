package exastencils.domain.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.deprecated.domain._
import exastencils.deprecated.l4._
import exastencils.domain._
import exastencils.domain.ir._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_HACK_DomainDecl

object L4_HACK_DomainDecl {
  var runningIndex = 0
}

// TODO: refactor - members of type Any, mixed declaration for different domain types, ConstVec usage, etc.
case class L4_HACK_DomainDecl(var name : String, var lower : Any, var upper : Any) extends L4_Node {
  //extends L4_KnowledgeDecl {
  def prettyprint(out : PpStream) = {
    (lower, upper) match {
      case (null, null)                       => out << s"Domain = fromFile($name)"
      case (l : L4_ConstVec, u : L4_ConstVec) => out << "Domain " << name << "< " << l << " to " << u << " >"
      case (lo : List[_], up : List[_])       =>
        (lo.head, up.head) match {
          case (_ : L4_ConstVec, _ : L4_ConstVec) =>
            val sep = lo.map(m => ", ").dropRight(1) :+ " >\n"
            out << "Domain " << name << "< "
            for (i <- lo.indices) { out << lo(i) << " to " << up(i) << sep(i) }
          //out << "Domain " << name << "< " << l(0) << " to " << u(0) << ", " << l(1) << " to " << u(1) << " ," << l(2) << " to " << u(2) << " >\n"
        }
      case _                                  => Logger.error("Unsupported: " + lower + " to " + upper)
    }
  }

  def progress : IR_Domain = {
    import L4_HACK_DomainDecl.runningIndex

    val index = runningIndex
    runningIndex += 1

    (lower, upper) match {
      case (null, null)                           =>
        FileInputGlobalDomain("global", index, DomainFileHeader.domainIdentifier.zipWithIndex.map {
          case (identifier, index) => FileInputDomain(identifier, index, FileInputDomainShape(identifier))
        }.toList)
      case (lo : List[_], up : List[_])           =>
        (lo.head, up.head) match {
          case (_ : L4_ConstVec2D, _ : L4_ConstVec2D) =>
            val rectUnionDomains : List[RectangularDomainShape] =
              lo.zip(up).map {
                case (li : L4_ConstVec2D, ui : L4_ConstVec2D) =>
                  RectangularDomainShape(deprecated_AABB(li.x, ui.x, li.y, ui.y, 0.0, 0.0))
              }
            ShapedDomain(name, index, ShapedDomainShape(rectUnionDomains))
        }
      case (l : L4_ConstVec2D, u : L4_ConstVec2D) => RectangularDomain(name, index, RectangularDomainShape(deprecated_AABB(l.x, u.x, l.y, u.y, 0, 0)))
      case (l : L4_ConstVec3D, u : L4_ConstVec3D) => RectangularDomain(name, index, RectangularDomainShape(deprecated_AABB(l.x, u.x, l.y, u.y, l.z, u.z)))
      case _                                      => RectangularDomain(name, index, RectangularDomainShape(deprecated_AABB()))
    }
  }
}

/// L4_ProcessFieldDeclarations

object L4_HACK_ProcessDomainDeclarations extends DefaultStrategy("Integrating L4 domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case domainDecl : L4_HACK_DomainDecl =>
      //domainDecl.addToKnowledge()
      IR_DomainCollection.objects += domainDecl.progress
      None // consume declaration statement
  })
}
