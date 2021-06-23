package exastencils.waLBerla.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Datatype
import exastencils.base.l4.L4_Function
import exastencils.base.l4.L4_Statement
import exastencils.base.l4.L4_UnitDatatype
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaLeveledFunction
import exastencils.waLBerla.ir.IR_WaLBerlaPlainFunction

case class L4_WaLBerlaFunction(
    var name : String,
    var level : Option[Int], // level of the function, plain if undefined
    var datatype: L4_Datatype,
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement]
) extends L4_Function {

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Function " << name << (if (level.isDefined) s"@$level" else "")
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L4_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation {
    if (level.isDefined)
      IR_WaLBerlaLeveledFunction(name, level.get, datatype.progress, parameters.map(_.progress), body.map(_.progress))
    else
      IR_WaLBerlaPlainFunction(name, datatype.progress, parameters.map(_.progress), body.map(_.progress))
  }
}
