package exastencils.waLBerla.ir.refinement

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.logger.Logger
import exastencils.util.ir.IR_AABB
import exastencils.waLBerla.ir.grid.IR_WaLBerlaAABB
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaPlainFunction
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaWrapperFunction
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes._

/// IR_WaLBerlaRefinementSelection

case class IR_WaLBerlaRefinementSelection(name : String, aabb : IR_AABB, refinementTargetLevel : Int) extends IR_KnowledgeObject {
  override def createDuplicate() : IR_KnowledgeObject = Logger.error("Trying to duplicate an ir refinement selection. This is currently unsupported.")

  def numDims = aabb.numDims
}

/// IR_WaLBerlaRefinementExclusion2D
// waLBerla always refines in an octree-fashion -> in 2D, we exclude child blocks spawned in z-direction to emulate quadtrees
case class IR_WaLBerlaRefinementExclusion2D() extends IR_WaLBerlaWrapperFunction {
  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    val setupBlock = IR_FunctionArgument("setupBlock", IR_ConstReferenceDatatype(WB_SetupBlock))
    val childAABB = IR_VariableAccess("childAABB", IR_WaLBerlaAABB.datatype)
    val parentAABB = IR_VariableAccess("parentAABB", IR_WaLBerlaAABB.datatype)

    var body : ListBuffer[IR_Statement] = ListBuffer()

    body += IR_VariableDeclaration(parentAABB,
      IR_MemberFunctionCallArrowWithDt(IR_MemberFunctionCall(setupBlock.access, "getFather"), "getAABB", IR_WaLBerlaAABB.datatype))
    body += IR_VariableDeclaration(childAABB,
      IR_MemberFunctionCallWithDt(setupBlock.access, "getAABB", IR_WaLBerlaAABB.datatype))

    val zMaxChild = IR_MemberFunctionCallWithDt(childAABB, "zMax", WB_RealType)
    val zMinChild = IR_MemberFunctionCallWithDt(childAABB, "zMax", WB_RealType)
    val zMaxParent = IR_MemberFunctionCallWithDt(parentAABB, "zMax", WB_RealType)

    body += IR_Return(IR_FunctionCall("std::fabs", zMaxChild - zMaxParent) < 1E-6 * (zMaxChild - zMinChild))

    IR_WaLBerlaPlainFunction(name, IR_BooleanDatatype, ListBuffer(setupBlock), body)
  }
  override def isInterfaceFunction : Boolean = false
  override def inlineIncludeImplementation : Boolean = false
  override def name : String = "refinementExclusionSelection2D"
}
