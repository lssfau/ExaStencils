package exastencils.datastructures.l4

import exastencils.datastructures._

case class Root(var fields : List[Statement], var statements : List[Statement]) extends Node {
  def this(statements : List[Statement]) = this(List(), statements)
}