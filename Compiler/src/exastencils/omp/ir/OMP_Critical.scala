package exastencils.omp.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream

/// OMP_Critical

object OMP_Critical {
  def apply(body : IR_Statement*) = new OMP_Critical(body.to[ListBuffer])

  var counter = 0
}

case class OMP_Critical(var body : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    import OMP_Critical.counter

    out << "#pragma omp critical"
    if (Knowledge.omp_nameCriticalSections) {
      out << " (section_" << counter << ")"
      counter += 1
    }

    out << '\n' << '{' << '\n'
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

/// OMP_PotentiallyCritical

object OMP_PotentiallyCritical {
  def apply(body : IR_Statement*) = new OMP_PotentiallyCritical(body.to[ListBuffer])
}

case class OMP_PotentiallyCritical(var body : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}

/// OMP_HandleCriticalSections

object OMP_HandleCriticalSections extends DefaultStrategy("Handle potentially critical omp sections") {
  this += new Transformation("Adding OMP critical pragmas", {
    case target : OMP_PotentiallyCritical =>
      if (Platform.omp_requiresCriticalSections)
        OMP_Critical(target.body)
      else
        target.body
  }, false)
}
