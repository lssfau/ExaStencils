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

package exastencils.datastructures

import scala.collection.GenTraversableOnce

/** The basic trait every matchable (in terms of [[exastencils.datastructures.Transformation]] matching) entity has to subclass. */
trait Node extends Annotatable with Product with Serializable {
  var location = SourceLocation()
}

final case class HelperNode(node : Node) extends Node

/** A type for returning more than a single [[exastencils.datastructures.Node]] per [[exastencils.datastructures.Transformation]]. */
final class NodeList(var nodes : GenTraversableOnce[Node])

package object ir {/* FIXME: this is only a placeholder until something reasonable is in place */ type StatementList = NodeList }
