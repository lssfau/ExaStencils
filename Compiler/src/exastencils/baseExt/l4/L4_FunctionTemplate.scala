package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.prettyprinting._

/// L4_FunctionTemplate

object L4_FunctionTemplate {
  def apply(name : String, templateArgs : List[String], functionArgs : List[L4_FunctionArgument], returntype : L4_Datatype, statements : List[L4_Statement]) =
    new L4_FunctionTemplate(name, templateArgs.to[ListBuffer], functionArgs.to[ListBuffer], returntype, statements.to[ListBuffer])
}

case class L4_FunctionTemplate(
    var name : String,
    var templateArgs : ListBuffer[String],
    var functionArgs : ListBuffer[L4_FunctionArgument],
    var returntype : L4_Datatype,
    var statements : ListBuffer[L4_Statement]) extends L4_Node with PrettyPrintable {

  override def prettyprint(out : PpStream) = {
    out << "FunctionTemplate " << name << " < " << templateArgs.mkString(", ") << " > ( "
    out <<< (functionArgs, ", ") << " )" << " : " << returntype << " {\n"
    out <<< (statements, "\n")
    out << "\n}"
  }
}
