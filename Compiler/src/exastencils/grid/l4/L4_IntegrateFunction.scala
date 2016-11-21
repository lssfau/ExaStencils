package exastencils.grid.l4

import scala.collection.mutable.HashSet

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

object L4_IntegrateFunctionAccess {
  def apply(name : String, datatype : L4_Datatype) =
    new L4_IntegrateFunctionAccess(name, None, datatype)
  def apply(name : String, level : Int, datatype : L4_Datatype) =
    new L4_IntegrateFunctionAccess(name, Some(level), datatype)
}

// TODO: pipe to ir integrate function access
case class L4_IntegrateFunctionAccess(var name : String, level : Option[Int], var datatype : L4_Datatype) extends L4_FunctionAccess
