package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.prettyprinting._

/// L3_FunctionTemplate

object L3_FunctionTemplate {
  def apply(name : String, templateArgs : Option[List[String]], functionArgs : Option[Option[List[L3_FunctionArgument]]],
      returnType : Option[L3_Datatype], statements : List[L3_Statement]) : L3_FunctionTemplate =
    L3_FunctionTemplate(name, returnType.getOrElse(L3_UnitDatatype), functionArgs.getOrElse(None).getOrElse(List()).to[ListBuffer],
      templateArgs.getOrElse(List()).to[ListBuffer], statements.to[ListBuffer])
}

case class L3_FunctionTemplate(
    var name : String,
    var returntype : L3_Datatype,
    var functionArgs : ListBuffer[L3_FunctionArgument],
    var templateArgs : ListBuffer[String],
    var statements : ListBuffer[L3_Statement]) extends L3_Statement {

  override def prettyprint(out : PpStream) = ???
  override def progress = ???
}
