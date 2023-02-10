//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.grid.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core._
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger

object IR_GridUtil {
  /// helper functions for shifting indices and accesses

  def offsetIndex(index : IR_ConstIndex, offset : Int, dim : Int) : IR_ConstIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetIndex(index : IR_ExpressionIndex, offset : IR_Expression, dim : Int) : IR_ExpressionIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetAccess(fieldAccess : IR_FieldLikeAccess, offset : Int, dim : Int) : IR_FieldLikeAccess = {
    val modAccess = Duplicate(fieldAccess)
    modAccess.index(dim) += offset
    modAccess
  }

  def offsetAccess(fieldAccess : IR_FieldLikeAccess, offset : IR_Expression, dim : Int) : IR_FieldLikeAccess = {
    val modAccess = Duplicate(fieldAccess)
    modAccess.index(dim) += offset
    modAccess
  }

  /// helper functions for projecting indices and accesses

  def projectIdx(index : IR_ExpressionIndex, dim : Int) = {
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
      case other                     => Logger.error(face)
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
      case 0     => "West"
      case 1     => "South"
      case 2     => "Bottom"
      case other => Logger.error(s"Unsupported dimension $other")
    }

    dimStringsToFace(stagDimStr, faceDimStr)
  }
}
