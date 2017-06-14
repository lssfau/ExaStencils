package exastencils.grid.l4

import scala.collection.mutable.HashSet

import exastencils.base.ir.IR_UserFunctionAccess
import exastencils.base.l4._

// L4_IntegrateFunctions

object L4_IntegrateFunctions {
  // TODO: add/ check call parameters?
  val functions = HashSet[String](
    "integrateOverEastFace", "integrateOverWestFace",
    "integrateOverNorthFace", "integrateOverSouthFace",
    "integrateOverTopFace", "integrateOverBottomFace",

    "integrateOverXStaggeredEastFace", "integrateOverXStaggeredNorthFace", "integrateOverXStaggeredTopFace",
    "integrateOverXStaggeredWestFace", "integrateOverXStaggeredSouthFace", "integrateOverXStaggeredBottomFace",

    "integrateOverYStaggeredEastFace", "integrateOverYStaggeredNorthFace", "integrateOverYStaggeredTopFace",
    "integrateOverYStaggeredWestFace", "integrateOverYStaggeredSouthFace", "integrateOverYStaggeredBottomFace",

    "integrateOverZStaggeredEastFace", "integrateOverZStaggeredNorthFace", "integrateOverZStaggeredTopFace",
    "integrateOverZStaggeredWestFace", "integrateOverZStaggeredSouthFace", "integrateOverZStaggeredBottomFace")

  def getValue(fctName : String) = Some(L4_UnknownDatatype)
  def exists(fctName : String) = functions.contains(fctName)
}

/// L4_IntegrateFunctionAccess

// TODO: pipe to ir integrate function access
case class L4_IntegrateFunctionAccess(var name : String, var level : Int, var datatype : L4_Datatype) extends L4_LeveledFunctionAccess {
  override def progress = IR_UserFunctionAccess(name, datatype.progress)
}
