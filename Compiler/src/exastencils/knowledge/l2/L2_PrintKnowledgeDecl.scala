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

package exastencils.knowledge.l2

import exastencils.domain.l2.L2_DomainCollection
import exastencils.field.l2._
import exastencils.fieldlike.l2.L2_FieldLikeCollections
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
    for (fieldCollection <- L2_FieldLikeCollections.collections) {
      fieldCollection.objects.foreach { obj =>
        obj.prettyprintDecl(out)
        out << "\n\n"
      }
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
