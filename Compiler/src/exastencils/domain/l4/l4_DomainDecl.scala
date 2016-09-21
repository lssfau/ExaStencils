package exastencils.domain.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.domain._
import exastencils.knowledge
import exastencils.knowledge.DomainCollection
import exastencils.knowledge.l4.L4_KnowledgeDecl
import exastencils.prettyprinting.PpStream
import exastencils.util.AABB

/// L4_DomainDecl

object L4_DomainDecl {
  var runningIndex = 0
}

// TODO: refactor - members of type Any, mixed declaration for different domain types, ConstVec usage, etc.
case class L4_DomainDecl(var name : String, var lower : Any, var upper : Any) extends L4_KnowledgeDecl {
  override def prettyprint(out : PpStream) = {
    (lower, upper) match {
      case (null, null)                 => out << s"Domain = fromFile($name) \n"
      case (l : ConstVec, u : ConstVec) => out << "Domain " << name << "< " << l << " to " << u << " >\n"
      case (lo : List[_], up : List[_]) => {
        (lo.head, up.head) match {
          case (_ : ConstVec, _ : ConstVec) => {
            val sep = lo.map(m => ", ").dropRight(1) :+ " >\n"
            out << "Domain " << name << "< "
            for (i <- lo.indices) { out << lo(i) << " to " << up(i) << sep(i) }
            //out << "Domain " << name << "< " << l(0) << " to " << u(0) << ", " << l(1) << " to " << u(1) << " ," << l(2) << " to " << u(2) << " >\n"
          }
        }
      }
    }
  }

  def progress : knowledge.Domain = {
    import L4_DomainDecl.runningIndex

    val index = runningIndex
    runningIndex += 1

    (lower, upper) match {
      case (null, null)                     => {
        knowledge.FileInputGlobalDomain("global", index, DomainFileHeader.domainIdentifier.zipWithIndex.map {
          case (identifier, index) => knowledge.FileInputDomain(identifier, index, FileInputDomainShape(identifier))
        }.toList)
      }
      case (lo : List[_], up : List[_])     => {
        (lo.head, up.head) match {
          case (_ : ConstVec2D, _ : ConstVec2D) => {
            val rectUnionDomains : List[RectangularDomainShape] =
              lo.zip(up).map {
                case (li : ConstVec2D, ui : ConstVec2D) =>
                  RectangularDomainShape(AABB(li.x, ui.x, li.y, ui.y, 0.0, 0.0))
              }
            knowledge.ShapedDomain(name, index, ShapedDomainShape(rectUnionDomains))
          }
        }
      }
      case (l : ConstVec2D, u : ConstVec2D) => knowledge.RectangularDomain(name, index, RectangularDomainShape(AABB(l.x, u.x, l.y, u.y, 0, 0)))
      case (l : ConstVec3D, u : ConstVec3D) => knowledge.RectangularDomain(name, index, RectangularDomainShape(AABB(l.x, u.x, l.y, u.y, l.z, u.z)))
      case _                                => knowledge.RectangularDomain(name, index, RectangularDomainShape(new AABB()))
    }
  }
  override def addToKnowledge() = ???
}

/// L4_ProcessFieldDeclarations

object L4_HACK_ProcessDomainDeclarations extends DefaultStrategy("Integrating L4 domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case domainDecl : L4_DomainDecl =>
      //domainDecl.addToKnowledge()
      DomainCollection.domains += domainDecl.progress
      None // consume declaration statement
  })
}