package exastencils.waLBerla.ir.field

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.waLBerla.ir.blockforest._
import exastencils.waLBerla.ir.interfacing._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes._

case class IR_WaLBerlaGetSizeForLevel(var level : Int) extends IR_WaLBerlaFutureLeveledFunction {

  private val returnType = IR_SpecialDatatype("Vector3< uint_t >")

  def getReference = IR_AddressOf(IR_VariableAccess(name + "_" + level, returnType))

  override def isInterfaceFunction : Boolean = false
  override def inlineImplementation : Boolean = false

  override def generateWaLBerlaFct() : IR_WaLBerlaLeveledFunction = {

    val block = new IR_WaLBerlaBlock("block", IR_ConstPointerDatatype(WB_IBlock))
    val blockForest = IR_WaLBerlaBlockForest()
    val cells = IR_VariableAccess("cells", returnType)

    val maxLevel = if (blockForest.maxLevelWaLBerlaField.isDefined) {
      blockForest.maxLevelWaLBerlaField.get.level
    } else {
      Knowledge.maxLevel
    }

    var params : ListBuffer[IR_FunctionArgument] = ListBuffer()
    params += IR_FunctionArgument("blocks", IR_ConstReferenceDatatype(IR_SharedPointerDatatype(WB_StructuredBlockStorage)))
    params += IR_FunctionArgument(block)

    var body : ListBuffer[IR_Statement] = ListBuffer()

    body += IR_ObjectInstantiation(cells, (0 until 3).map(d => blockForest.getNumberOfCells(d, block)) : _*)

    if (level != maxLevel) {
      val lvlDiff = maxLevel - level

      for (d <- Knowledge.dimensions) {
        body += IR_Assert(IR_ArrayAccess(cells, d) Mod (2 Pow math.abs(lvlDiff)) EqEq 0,
          ListBuffer(IR_StringConstant("Coarsening with factors other than two")), IR_FunctionCall("exit", 1))
      }

      for (d <- Knowledge.dimensions) {
        body += IR_Assignment(IR_ArrayAccess(cells, d),
          if (lvlDiff > 0) IR_RightShift(IR_ArrayAccess(cells, d), lvlDiff)
          else IR_LeftShift(IR_ArrayAccess(cells, d), lvlDiff))
      }
    }

    body += IR_Return(cells)

    IR_WaLBerlaLeveledFunction(name, level, returnType, params, body)
  }

  override def name : String = "calculateSize"
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
