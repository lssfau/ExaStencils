package exastencils.datastructures.l4

import exastencils.datastructures._

case class Root(var fields : List[FieldDeclarationStatement], var statements : List[Statement]) extends Node {
  def this(statements : List[Statement]) = this(List(), statements)

  def getFieldByIdentifier(identifier : String, level : Int) : Option[FieldDeclarationStatement] = {
    fields.find(f => f.name == identifier && f.level.getOrElse(-1) == level)
  }
}
