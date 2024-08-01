package exastencils.waLBerla.ir.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_StdArrayDatatype
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

abstract class IR_WaLBerlaCommScheme extends IR_WaLBerlaInterfaceMember(false, true, false) {
  def wbField : IR_WaLBerlaField
  def slot : IR_Expression

  def blockDataID : IR_WaLBerlaBlockDataID
  def blockForest : IR_WaLBerlaBlockForest

  def level = wbField.level
  def numSlots = wbField.numSlots

  override def minLevel : Int = blockDataID.minLevel
  override def maxLevel : Int = blockDataID.maxLevel

  def basetype : IR_Datatype

  override def resolveDatatype() : IR_Datatype = {
    var dt : IR_Datatype = basetype

    if (numSlots > 1)
      dt = IR_StdArrayDatatype(dt, numSlots)

    dt
  }

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess()

  private def resolveAccess() : IR_Expression = resolveAccess(resolveMemberBaseAccess(), IR_NullExpression, level, IR_NullExpression)

  def createUniformPackInfo() : IR_Expression

  def addPackInfo() = IR_MemberFunctionCallArrow(resolveAccess(), "addPackInfo", createUniformPackInfo())

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, level : IR_Expression, neigh : IR_Expression) = {
    var access = super.resolveAccess(baseAccess, IR_NullExpression, level, IR_NullExpression)

    if (numSlots > 1)
      access = IR_ArrayAccess(access, slot)

    access
  }

  def commSchemeNecessaryWrapper(stmts : ListBuffer[IR_Statement]) : IR_IfCondition = {
    if (Knowledge.waLBerla_useGridPartFromExa)
      IR_IfCondition(Knowledge.domain_numFragmentsTotal > 1, stmts)
    else
      IR_IfCondition(blockForest.getNumberOfAllRootBlocks() > 1 OrOr blockForest.getNumberOfAllLocalBlocks() > 1, stmts)
  }

  def communicate() : IR_Statement = {
    if (Knowledge.waLBerla_useRefinement && wbField.layout.communicatesGhosts) {
        if (!Knowledge.waLBerla_useConservingRefinementPackInfo) {
          // (linear)/quadratic extrapolation/interpolation scheme requires at least one ghost layer
          val expectedNrOfGhosts = wbField.layout.layoutsPerDim.forall(e => e.numGhostLayersLeft >= 1 && e.numGhostLayersRight >= 1)
          if (!expectedNrOfGhosts)
            Logger.error("Generated CPU comm schemes with refinement enabled requires at least ghost layers for quadratic interp schemes. Error in layout: " + wbField.layout.name)
        } else {
          // constant/linear interpolation scheme requires two ghost layers per side
          val expectedNrOfGhosts = wbField.layout.layoutsPerDim.forall(e => e.numGhostLayersLeft == 2 && e.numGhostLayersRight == 2)
          if (!expectedNrOfGhosts)
            Logger.error("Generated CPU comm schemes with refinement enabled require two ghost layers for constant/linear interp schemes. Error in layout: " + wbField.layout.name)
        }
    }

    // deref and call functor
    val deref = IR_DerefAccess(resolveAccess() match {
      case base : IR_Access => base
      case _                => Logger.error("Invalid access used for de-referencing a IR_WaLBerlaCommScheme instance.")
    })
    val comm = IR_FunctorCall(deref)

    commSchemeNecessaryWrapper(ListBuffer(comm))
  }
}
