package exastencils.grid.l3

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.core._
import exastencils.field.l3.L3_FieldAccess
import exastencils.logger.Logger

object L3_GridUtil {
  /// helper functions for shifting indices and accesses

  def offsetIndex(index : L3_ConstIndex, offset : Int, dim : Int) : L3_ConstIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetIndex(index : L3_ExpressionIndex, offset : L3_Expression, dim : Int) : L3_ExpressionIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetAccess(fieldAccess : L3_FieldAccess, offset : Int, dim : Int) : L3_FieldAccess = {
    val modAccess = Duplicate(fieldAccess)
    if (modAccess.offset.isEmpty)
      modAccess.offset = Some(L3_ConstIndex(Array.fill(fieldAccess.target.numDimsGrid)(0)))
    modAccess.offset.get(dim) += offset
    modAccess
  }

  /// helper functions for projecting indices and accesses

  def projectIdx(index : L3_ExpressionIndex, dim : Int) = {
    val modIndex = Duplicate(index)
    for (i <- 0 until modIndex.length; if dim != i)
      modIndex(i) *= 0 // keeps null entries, sets non-null entries to 0
    modIndex
  }

  /// helper functions handling face aliases
  def faceToDimStrings(face : String) = {
    val pattern = """(XStaggered|YStaggered|ZStaggered)?(East|West|North|South|Top|Bottom)Face""".r
    face match {
      case pattern(null, faceDim)    => (None, faceDim)
      case pattern(stagDim, faceDim) => (Some(stagDim), faceDim)
    }
  }

  def faceToDims(face : String) = {
    val (stagDimStr, faceDimStr) = faceToDimStrings(face)

    val stagDim = stagDimStr match {
      case None               => None
      case Some("XStaggered") => Some(0)
      case Some("YStaggered") => Some(1)
      case Some("ZStaggered") => Some(2)
      case Some(other)        => Logger.error(s"Unsupported identifier $other")
    }

    val faceDim = faceDimStr match {
      case "East" | "West"   => 0
      case "North" | "South" => 1
      case "Top" | "Bottom"  => 2
      case other             => Logger.error(s"Unsupported identifier $other")
    }

    (stagDim, faceDim)
  }

  def offsetForFace(face : String) = {
    val (_, faceDim) = faceToDimStrings(face)

    faceDim match {
      case "West" | "South" | "Bottom" => 0
      case "East" | "North" | "Top"    => 1
    }
  }

  def dimStringsToFace(stagDim : Option[String], faceDim : String) = {
    s"${ stagDim.getOrElse("") }${ faceDim }Face"
  }

  def dimsToFace(stagDim : Option[Int], faceDim : Int) = {
    val stagDimStr = stagDim match {
      case None        => None
      case Some(0)     => Some("XStaggered")
      case Some(1)     => Some("YStaggered")
      case Some(2)     => Some("ZStaggered")
      case Some(other) => Logger.error(s"Unsupported dimension $other")
    }

    val faceDimStr = faceDim match {
      case 0     => "East"
      case 1     => "North"
      case other => Logger.error(s"Unsupported dimension $other")
    }

    dimStringsToFace(stagDimStr, faceDimStr)
  }
}
