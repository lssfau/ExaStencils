package exastencils.omp

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

trait OMP_PotentiallyCritical
trait OMP_PotentiallyParallel { var reduction : Option[Reduction]; var collapse = 1 }

case class OMP_Critical(var body : Scope) extends Statement {
  def this(body : Statement) = this(new Scope(body))
  def this(body : ListBuffer[Statement]) = this(new Scope(body))

  def cpp : String = {
    s"#pragma omp critical\n" + body.cpp
  }
}

case class OMP_ParallelFor(var body : ForLoopStatement, var addOMPStatements : Expression, var collapse : Int = 1) extends Statement {
  def cpp : String = {
    s"#pragma omp parallel for schedule(static) num_threads(${Knowledge.domain_fragLength.max(Knowledge.domain_numFragsPerBlock)}) ${addOMPStatements.cpp}${if (1 != collapse && Knowledge.omp_version >= 3 && Knowledge.omp_useCollapse) s" collapse($collapse)" else ""}\n" +
      body.cpp
  }
}