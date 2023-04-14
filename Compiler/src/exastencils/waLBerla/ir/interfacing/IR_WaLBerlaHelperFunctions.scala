package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaGetSizeForLevel
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_RealType
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_UintType

object IR_WaLBerlaHelperFunctionCollection {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  for (lvl <- Knowledge.levels)
    functions += IR_WaLBerlaGetSizeForLevel(lvl)
  functions += IR_WaLBerlaAddFieldToStorageWrapper()
}

trait IR_WaLBerlaWrapperFunction extends IR_WaLBerlaFuturePlainFunction {
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint
}

case class IR_WaLBerlaAddFieldToStorageWrapper() extends IR_WaLBerlaWrapperFunction {
  def blockForest = IR_WaLBerlaBlockForest()
  def blocks = blockForest.ctorParameter
  def identifier = IR_FunctionArgument("identifier", IR_StringDatatype)
  def level = IR_FunctionArgument("level", IR_IntegerDatatype)
  def initVal = IR_FunctionArgument("initVal", WB_RealType)
  def fieldLayout = IR_FunctionArgument("layout", IR_SpecialDatatype("field::Layout"))
  def nrOfGhostLayers = IR_FunctionArgument("nrOfGhostLayers", WB_UintType)
  def useStdFieldAlloc = IR_FunctionArgument("useStdFieldAlloc", IR_BooleanDatatype)

  // templated, non-interface function
  val fieldTemplate = "Field_T"
  functionQualifiers += s"template < typename $fieldTemplate >"
  override def inlineIncludeImplementation : Boolean = true
  override def isInterfaceFunction : Boolean = false

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    // add deps
    IR_WaLBerlaCollection.get.addExternalDependency("field/Field.h")
    IR_WaLBerlaCollection.get.addExternalDependency("field/AddToStorage.h")

    var params : ListBuffer[IR_FunctionArgument] = ListBuffer()
    params += blocks
    params += identifier
    params += level
    params += initVal
    params += fieldLayout
    params += nrOfGhostLayers
    params += useStdFieldAlloc

    val cases = ListBuffer[IR_Case]()

    val funcRefName = s"field::addToStorage< $fieldTemplate >"

    // set up jump table for different level cases
    for (lvl <- Knowledge.levels) {
      val func = IR_WaLBerlaGetSizeForLevel(lvl)
      if (!IR_WaLBerlaCollection.get.functions.contains(func))
        IR_WaLBerlaCollection.get.functions += func

      // calc size function for wb fields
      def calcSize(lvl : Int) = IR_VariableAccess(s"calcSize_$lvl", "auto")

      val args = ListBuffer[IR_Expression]()
      args += blocks.access // blockstorage
      args += identifier.access // identifier
      args += calcSize(lvl) // calculateSize
      args += initVal.access // initValue
      args += fieldLayout.access // layout
      args += nrOfGhostLayers.access // nrOfGhostLayers

      // use constructor with StdFieldAlloc allocator to avoid inconsistencies between waLBerla's automatic padding and exa's padding
      val newArgs = Duplicate(args)
      newArgs += IR_BooleanConstant(false) // alwaysInitialize
      newArgs += IR_Native(s"std::function< void ( $fieldTemplate * field, IBlock * const block ) >()") // initFunction
      newArgs += IR_Native("Set<SUID>::emptySet()") // requiredSelectors
      newArgs += IR_Native("Set<SUID>::emptySet()") // incompatibleSelectors
      newArgs += IR_Native(s"make_shared < field::StdFieldAlloc< typename $fieldTemplate::value_type > >()") // alloc

      cases += IR_Case(lvl,
        ListBuffer[IR_Statement](
          // set up lambda for calculating size
          IR_VariableDeclaration(calcSize(lvl),
            IR_FunctionCall(IR_ExternalFunctionReference("std::bind"), func.getReference,
              // use placeholders for:
              IR_Native("std::placeholders::_1"), // blockstorage
              IR_Native("std::placeholders::_2") // iblock
            )
          ),
          IR_IfCondition(useStdFieldAlloc.access,
            IR_Return(IR_FunctionCall(IR_ExternalFunctionReference(funcRefName), newArgs)),
            IR_Return(IR_FunctionCall(IR_ExternalFunctionReference(funcRefName), args)))
        )
      )
    }
    val returnType = WB_BlockDataID

    IR_WaLBerlaPlainFunction(name, returnType, params, ListBuffer(IR_Switch(level.access, cases)))
  }

  override def name : String = s"addToStorageWrapper"
}
