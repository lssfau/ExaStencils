package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config.Knowledge
import exastencils.deprecated.domain.ir._
import exastencils.grid.ir._

case class IR_DomainFunctions() extends IR_FunctionCollection(
  "Domains/DomainGenerated",
  ListBuffer(),
  ListBuffer("Globals/Globals.h", "CommFunctions/CommFunctions.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"

  if (Knowledge.domain_rect_generate) {
    functions += IR_InitGeneratedDomain()

    // assemble init statements for applicable virtual fields
    val initStmts = ListBuffer[IR_Statement]()

    var dependencies = IR_VirtualFieldCollection.objects.map(vf => (vf, vf.generateInitCodeDependsOn()))
    while (dependencies.nonEmpty) {
      val (canBeDone, waiting) = dependencies.partition(_._2.isEmpty)
      initStmts ++= canBeDone.flatMap(_._1.generateInitCode())
      dependencies = waiting.map(e => (e._1, e._2.filterNot(canBeDone.map(_._1).contains)))
    }

    functions += IR_PlainFunction("initGeometry", IR_UnitDatatype, initStmts)
  } else {
    externalDependencies += ("iostream", "fstream")
    val rvTemplateFunc = IR_PlainFunction(
      s"readValue",
      IR_SpecialDatatype("template <class T> T"),
      ListBuffer[IR_FunctionArgument](
        IR_FunctionArgument("memblock", IR_SpecialDatatype("char*&")),
        IR_FunctionArgument("title = \"\"", IR_SpecialDatatype("std::string"))),
      ListBuffer[IR_Statement](
        IR_VariableDeclaration(IR_IntegerDatatype, "size", "sizeof(T)"),
        IR_VariableDeclaration(IR_CharDatatype, "bytes[size]"),
        IR_ForLoop("int j = 0", " j < size ", "++j", "bytes[size-1-j] = memblock[j]"),
        "memblock+=size",
        IR_Return("*(T *)&bytes")))
    rvTemplateFunc.isHeaderOnly = true // annotate("isTemplate")
    functions += rvTemplateFunc
    functions += IR_SetValues()
    functions += IR_InitDomainFromFragmentFile()
  }
}
