package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config.Knowledge
import exastencils.grid.ir._
import exastencils.logger.Logger

case class IR_DomainFunctions() extends IR_FunctionCollection(
  "Domains/DomainGenerated",
  ListBuffer(),
  ListBuffer("Globals/Globals.h", "CommFunctions/CommFunctions.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"

  if (Knowledge.domain_rect_generate)
    functions += IR_InitGeneratedDomain()
  else if (Knowledge.domain_readFromFile)
    functions += IR_InitDomainFromFile()
  else
    Logger.error("Unsupported domain configuration")

  // assemble init statements for applicable virtual fields
  { // scope to avoid initStmts being a member which is later targeted by transformations
    val initStmts = ListBuffer[IR_Statement]()

    var dependencies = IR_VirtualFieldCollection.objects.map(vf => (vf, vf.generateInitCodeDependsOn()))
    while (dependencies.nonEmpty) {
      val (canBeDone, waiting) = dependencies.partition(_._2.isEmpty)
      initStmts ++= canBeDone.flatMap(_._1.generateInitCode())
      dependencies = waiting.map(e => (e._1, e._2.filterNot(canBeDone.map(_._1).contains)))
    }

    functions += IR_PlainFunction("initGeometry", IR_UnitDatatype, initStmts)
  }
}
