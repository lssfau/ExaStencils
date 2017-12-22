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
