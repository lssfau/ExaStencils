package exastencils.omp

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

trait OMP_PotentiallyCritical
trait OMP_PotentiallyParallel { var reduction : Option[Reduction]; var collapse = 1 }

case class OMP_Critical(var body : Any) extends Statement {
  // FIXME: most constructs don't need to be protected on JuQueen as a thread-safe MPI implementation exists. How should this be incorporated?

  def cpp : String = {
    s"#pragma omp critical\n{ " +
      (body match {
        case prettyPrintable : CppPrettyPrintable => prettyPrintable.cpp
        case buf : ListBuffer[_] => buf.map(stat => stat match { // TODO: Buffer support is currently not tested!
          case prettyPrintable : CppPrettyPrintable => prettyPrintable.cpp
          case _                                    => "NON_PRINTABLE NODE ENCOUNTERED!"
        })
        case _ => "NON_PRINTABLE NODE ENCOUNTERED!"
      }) +
      s" }"
  }
}

case class OMP_ParallelFor(var body : ForLoopStatement, var addOMPStatements : Expression, var collapse : Int = 1) extends Statement {
  def cpp : String = {
    s"#pragma omp parallel for schedule(static) num_threads(${Knowledge.domain_fragLength.max(Knowledge.domain_numFragsPerBlock)}) ${addOMPStatements.cpp}${if (1 != collapse && Knowledge.omp_version >= 3 && Knowledge.omp_useCollapse) s" collapse($collapse)" else ""}\n" +
      body.cpp
  }
}