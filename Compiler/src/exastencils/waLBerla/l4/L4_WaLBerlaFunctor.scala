package exastencils.waLBerla.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Function
import exastencils.base.l4.L4_Statement
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaFunctor

// TODO currently only usable within functions: allow declaration (outside) including levels

case class L4_WaLBerlaFunctor(
    var name : String,
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement]
) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Functor " << name
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation {
    IR_WaLBerlaFunctor(name, parameters.map(_.progress), body.map(_.progress))
  }
}
