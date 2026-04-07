//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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
    // TODO: Use FieldLikeCollections instead or FieldCollection
    L1_FieldCollection.objects.foreach { obj =>
      obj.prettyprintDecl(out)
      out << "\n\n"
    }

    out << "// field combinations\n\n"
    L1_FieldCombinationCollection.objects.foreach { obj =>
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
