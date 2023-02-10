package exastencils.parallelization.api.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Statement
import exastencils.prettyprinting.PpStream

/// OMP_Single

case class OMP_Single(var body : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "\n#pragma omp single\n"
    out << "{\n"
    out <<< body
    out << "\n}\n"
  }
}
