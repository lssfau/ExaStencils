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

package exastencils.base

import scala.language.experimental.macros
import scala.reflect.macros._

/// ProgressLocation

object ProgressLocation {
  def apply[T /*<: exastencils.datastructures.Node*/ ](node : T) : T = macro progressImpl[T]

  def isInNodeScope(c : blackbox.Context) = {
    var inNode = false

    var enclosing = c.internal.enclosingOwner
    while (!inNode && enclosing != c.universe.NoSymbol) {
      if (enclosing.isClass)
        if (enclosing.asClass.baseClasses.exists(_.fullName == "exastencils.datastructures.Node"))
          inNode = true

      enclosing = enclosing.owner
    }

    inNode
  }

  def progressImpl[T](c : blackbox.Context)(node : c.Expr[T]) : c.Expr[T] = {
    import c.universe._

    var applicable = true

    // check if location is available
    applicable &&= isInNodeScope(c)

    // check if T is subclass of node
    // TODO

    val result = if (!applicable) {
      q"""$node"""
    } else {
      val tmp = Ident(TermName(c.freshName("tmp")))
      q"""{
        val $tmp = $node
        $tmp.location = location
        $tmp
      }"""
    }

    c.Expr[T](result)
  }
}
