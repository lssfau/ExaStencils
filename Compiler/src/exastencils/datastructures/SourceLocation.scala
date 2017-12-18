package exastencils.datastructures

import scala.util.parsing.input.Position

import exastencils.core.Duplicate

/// SourceLocation

object SourceLocation {
  Duplicate.registerImmutable(classOf[SourceLocation], true)
}

case class SourceLocation(var position : Option[Position] = None, var fileName : Option[String] = None) {
  // init companion object
  SourceLocation

  def toAppendString = {
    var s = ""

    if (position.isDefined)
      s += " at location " + position.get
    if (fileName.isDefined)
      s += " in file " + fileName.get
    if (position.isDefined)
      s += ":\n" + position.get.longString

    if (s.nonEmpty)
      s = " ;" + s

    s
  }
}
