package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_FunctionTemplate
import exastencils.prettyprinting._

/// L3_FunctionTemplate

object L3_FunctionTemplate {
  def apply(name : String, datatype : Option[L3_Datatype], templateArgs : Option[List[String]],
      functionArgs : Option[Option[List[L3_Function.Argument]]], statements : List[L3_Statement]) : L3_FunctionTemplate =
    L3_FunctionTemplate(name, datatype.getOrElse(L3_UnitDatatype), templateArgs.getOrElse(List()).to[ListBuffer],
      functionArgs.getOrElse(None).getOrElse(List()).to[ListBuffer], statements.to[ListBuffer])
}

case class L3_FunctionTemplate(
    var name : String,
    var datatype : L3_Datatype,
    var templateArgs : ListBuffer[String],
    var functionArgs : ListBuffer[L3_Function.Argument],
    var statements : ListBuffer[L3_Statement]) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    out << "FunctionTemplate " << name << " < " << templateArgs.mkString(", ") << " > ( "
    out <<< (functionArgs, ", ") << " )" << " : " << datatype << " {\n"
    out <<< (statements, "\n")
    out << "\n}"
  }

  override def progress = ProgressLocation(L4_FunctionTemplate(name, datatype.progress, templateArgs, functionArgs.map(_.progress), statements.map(_.progress)))
}
