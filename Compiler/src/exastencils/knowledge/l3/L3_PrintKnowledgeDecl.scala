package exastencils.knowledge.l3

import exastencils.domain.l3.L3_DomainCollection
import exastencils.field.l3._
import exastencils.operator.l3._
import exastencils.prettyprinting.PpStream
import exastencils.solver.l3.L3_EquationCollection

object L3_PrintKnowledgeDecl {
  def apply(out : PpStream) = {
    out << "// domain declarations\n\n"
    L3_DomainCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// field declarations\n\n"
    L3_FieldCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// field combinations\n"
    L3_FieldCombinationCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// stencil declarations\n\n"
    L3_StencilCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// stencil field declarations\n\n"
    L3_StencilFieldCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// equations declarations\n\n"
    L3_EquationCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }
  }
}
