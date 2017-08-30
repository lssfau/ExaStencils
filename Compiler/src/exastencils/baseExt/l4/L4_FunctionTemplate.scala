package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_FunctionTemplate

object L4_FunctionTemplate {
  def apply(name : String, datatype : L4_Datatype, templateArgs : List[String], functionArgs : List[L4_Function.Argument], statements : List[L4_Statement]) =
    new L4_FunctionTemplate(name, datatype, templateArgs.to[ListBuffer], functionArgs.to[ListBuffer], statements.to[ListBuffer])
}

case class L4_FunctionTemplate(
    var name : String,
    var datatype : L4_Datatype,
    var templateArgs : ListBuffer[String],
    var functionArgs : ListBuffer[L4_Function.Argument],
    var statements : ListBuffer[L4_Statement]) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "FunctionTemplate " << name << " < " << templateArgs.mkString(", ") << " > ( "
    out <<< (functionArgs, ", ") << " )" << " : " << datatype << " {\n"
    out <<< (statements, "\n")
    out << "\n}"
  }

  override def progress = Logger.error("Trying to progress L4_FunctionTemplate; unsupported")
}
