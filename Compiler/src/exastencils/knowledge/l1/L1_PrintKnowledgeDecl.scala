package exastencils.knowledge.l1

import exastencils.domain.l1.L1_DomainCollection
import exastencils.field.l1._
import exastencils.operator.l1._
import exastencils.prettyprinting.PpStream
import exastencils.solver.l1.L1_EquationCollection

object L1_PrintKnowledgeDecl {
  def apply(out : PpStream) = {
    out << "// domain declarations\n\n"
    L1_DomainCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// field declarations\n\n"
    L1_FieldCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// operator declarations\n\n"
    L1_OperatorCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// equations declarations\n\n"
    L1_EquationCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }
  }
}
