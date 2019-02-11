package exastencils.knowledge.l2

import exastencils.domain.l2.L2_DomainCollection
import exastencils.field.l2._
import exastencils.operator.l2._
import exastencils.prettyprinting.PpStream
import exastencils.solver.l2.L2_EquationCollection

object L2_PrintKnowledgeDecl {
  def apply(out : PpStream) = {
    out << "// domain declarations\n\n"
    L2_DomainCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// field declarations\n\n"
    L2_FieldCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// field combinations\n\n"
    L2_FieldCombinationCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// stencil declarations\n\n"
    L2_StencilCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// stencil field declarations\n\n"
    L2_StencilFieldCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// equations declarations\n\n"
    L2_EquationCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }
  }
}
