package exastencils.grid.l4

import scala.collection.mutable.HashSet

import exastencils.base.l4._

// L4_EvalFunctions

object L4_EvalFunctions {
  // TODO: add/ check call parameters?
  val functions = HashSet[String](
    "evalAtEastFace", "evalAtWestFace",
    "evalAtNorthFace", "evalAtSouthFace",
    "evalAtTopFace", "evalAtBottomFace",

    "evalAtXStaggeredEastFace", "evalAtXStaggeredNorthFace", "evalAtXStaggeredTopFace",
    "evalAtXStaggeredWestFace", "evalAtXStaggeredSouthFace", "evalAtXStaggeredBottomFace",

    "evalAtYStaggeredEastFace", "evalAtYStaggeredNorthFace", "evalAtYStaggeredTopFace",
    "evalAtYStaggeredWestFace", "evalAtYStaggeredSouthFace", "evalAtYStaggeredBottomFace",

    "evalAtZStaggeredEastFace", "evalAtZStaggeredNorthFace", "evalAtZStaggeredTopFace",
    "evalAtZStaggeredWestFace", "evalAtZStaggeredSouthFace", "evalAtZStaggeredBottomFace")

  def getValue(fctName : String) = Some(L4_UnknownDatatype)
  def exists(fctName : String) = functions.contains(fctName)
}

/// L4_EvalFunctionAccess

object L4_EvalFunctionAccess {
  def apply(name : String, datatype : L4_Datatype) =
    new L4_EvalFunctionAccess(name, None, datatype)
  def apply(name : String, level : Int, datatype : L4_Datatype) =
    new L4_EvalFunctionAccess(name, Some(level), datatype)
}

// TODO: pipe to ir eval function access
case class L4_EvalFunctionAccess(var name : String, level : Option[Int], var datatype : L4_Datatype) extends L4_FunctionAccess
