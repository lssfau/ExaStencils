package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnitDatatype
import exastencils.config.Knowledge
import exastencils.field.ir

object IR_WaLBerlaInitIndexIVs {
  val name : String = "initIndexIVs"
}

case class IR_WaLBerlaInitIndexIVs() extends IR_WaLBerlaFuturePlainFunction {
  override def name : String = IR_WaLBerlaInitIndexIVs.name
  override def prettyprint_decl() : String = prettyprint()
  override def name_=(newName : String) : Unit = name = newName

  val possibleIdStrings = List(
    "PLB", "PLE","PRB", "PRE",
    "GLB", "GLE", "GRB", "GRE",
    "DLB", "DLE", "DRB", "DRE",
    "IB" , "IE" , "TOT"
  )

  val numDims : Int = Knowledge.dimensionality

  override def generateFct() : IR_WaLBerlaPlainFunction = {
    var body : ListBuffer[IR_Statement] = ListBuffer()

    IR_WaLBerlaFieldCollection.objects.foreach { field =>
      for (id <- possibleIdStrings)
        for (dim <- 0 until numDims)
          body += IR_Assignment(ir.IR_IV_IndexFromField(field.layout.name, field.level, id, dim), field.layout.defIdxById(id, dim))
    }
    body = ListBuffer(IR_WaLBerlaLoopOverBlocks(body))

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}
