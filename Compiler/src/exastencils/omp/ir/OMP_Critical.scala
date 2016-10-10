package exastencils.omp.ir

import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.parallelization.ir.IR_PotentiallyCritical
import exastencils.prettyprinting.PpStream

/// OMP_Critical

object OMP_Critical {
  var counter = 0
}

case class OMP_Critical(var body : IR_PotentiallyCritical) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    import OMP_Critical.counter

    out << "#pragma omp critical"
    if (Knowledge.omp_nameCriticalSections) {
      out << " (section_" << counter << ")"
      counter += 1
    }

    out << '\n' << body
  }
}

/// OMP_AddCriticalSections

object OMP_AddCriticalSections extends DefaultStrategy("Resolve potentially critical omp sections") {
  this += new Transformation("Adding OMP critical pragmas", {
    case target : IR_PotentiallyCritical => OMP_Critical(target)
  }, false) // turn off recursion due to wrapping mechanism
}
