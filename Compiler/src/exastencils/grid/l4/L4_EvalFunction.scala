package exastencils.grid.l4

import scala.collection.mutable.HashSet

import exastencils.base.ir._
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

// TODO: pipe to ir eval function access
case class L4_EvalFunctionAccess(var name : String, var level : Int, var datatype : L4_Datatype) extends L4_LeveledFunctionAccess {
  override def progress = IR_UserFunctionAccess(name, datatype.progress)
}
