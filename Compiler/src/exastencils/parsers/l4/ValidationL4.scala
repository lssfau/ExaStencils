package exastencils.parsers.l4

import exastencils.core.Logger
import exastencils.datastructures._
import exastencils.datastructures.l4._

object ValidationL4 {
  val s = DefaultStrategy("Validate L4 Input")

  s += Transformation("Check identifiers names", {
    case x : Identifier if (x.name.startsWith("_")) => Logger.error(s"""Illegal identifier "${x.name}": Identifiers may not start with '_'""")
  })

  def apply() = s.apply()
}