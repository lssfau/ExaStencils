package exastencils.knowledge.l4

import exastencils.domain.l4.L4_DomainCollection
import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ExternalFieldCollection
import exastencils.operator.l4.L4_StencilCollection
import exastencils.prettyprinting.PpStream
import exastencils.stencil.l4._

object L4_PrintKnowledgeDecl {
  def apply(out : PpStream) = {
    out << "// domain declarations\n\n"
    L4_DomainCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// field layout declarations\n\n"
    L4_FieldLayoutCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// field declarations\n\n"
    L4_FieldCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// external field declarations\n\n"
    L4_ExternalFieldCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// stencil declarations\n\n"
    L4_StencilCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// stencil field declarations\n\n"
    L4_StencilFieldCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }
  }
}
