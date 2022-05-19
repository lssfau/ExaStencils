package exastencils.waLBerla.ir.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field._
import exastencils.waLBerla.ir.interfacing._
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil._

object IR_WaLBerlaInitCommSchemes {
  var fctName : String = "initCommSchemes"
}

case class IR_WaLBerlaInitCommSchemes() extends IR_WaLBerlaFuturePlainFunction {
  override def name : String = IR_WaLBerlaInitCommSchemes.fctName
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint

  override def isInterfaceFunction : Boolean = true

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {

    val blockForest = IR_WaLBerlaBlockForest()
    val wbFieldsPerLevel = IR_WaLBerlaFieldCollection.objects.groupBy(_.codeName).map(_._2.head).to[ListBuffer] // find unique wb fields per level
      .sortBy(_.level).sortBy(_.name)

    var body = ListBuffer[IR_Statement]()

    // init comm scheme array
    for (wbf <- wbFieldsPerLevel) {
      val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
      val commScheme  = IR_WaLBerlaCommScheme(wbf, slotIt)

      body += IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt),
        IR_Assignment(commScheme.resolveAccess(), make_unique(commScheme.basetype.resolveBaseDatatype.prettyprint, blockForest)))
    }

    // add pack info
    for (wbf <- wbFieldsPerLevel) {
      val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
      val commScheme  = IR_WaLBerlaCommScheme(wbf, slotIt)

      body += IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt),
        commScheme.addPackInfo() : IR_Statement)
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}
