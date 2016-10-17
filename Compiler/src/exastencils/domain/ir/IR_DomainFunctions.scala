package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config.Knowledge
import exastencils.deprecated.domain.ir._
import exastencils.grid.GridGeometry

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
    functions += IR_Function(IR_UnitDatatype, "initGeometry", GridGeometry.getGeometry.generateInitCode())
  } else {
    externalDependencies += ("iostream", "fstream")
    val rvTemplateFunc = IR_Function(
      IR_SpecialDatatype("template <class T> T"),
      s"readValue",
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
