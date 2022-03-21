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

package exastencils.field.l4

import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.field.ir._
import exastencils.fieldlike.l4.L4_FieldLayoutLike
import exastencils.fieldlike.l4.L4_FieldLayoutLikeAccess
import exastencils.grid.l4._
import exastencils.prettyprinting._

case class L4_FieldLayout(
    var name : String, // will be used to find the layout
    var level : Int, // the level the field lives on
    var numDimsGrid : Int, // the number of dimensions of the grid
    var datatype : L4_Datatype,
    var localization : L4_Localization,
    var ghostLayers : L4_ConstIndex,
    var communicatesGhosts : Boolean,
    var duplicateLayers : L4_ConstIndex,
    var communicatesDuplicated : Boolean,
    var innerPoints : L4_ConstIndex) extends L4_FieldLayoutLike[IR_FieldLayout] {

  override def createDuplicate() : L4_FieldLayout = {
    L4_FieldLayout(name, level, numDimsGrid, Duplicate(datatype), Duplicate(localization), Duplicate(ghostLayers),
      communicatesGhosts, Duplicate(duplicateLayers), communicatesDuplicated, Duplicate(innerPoints))
  }

  override def prettyprintDecl(out : PpStream) = {
    out << "Layout " << name << "< "
    out << datatype << ", "
    out << localization
    out << " >" << "@(" << level << ") {\n"
    //FIXME: out << "innerPoints = " << innerPoints <<  "\n"
    out << "duplicateLayers = " << duplicateLayers << (if (communicatesDuplicated) " with communication\n" else "\n")
    out << "ghostLayers = " << ghostLayers << (if (communicatesGhosts) " with communication\n" else "\n")
    out << "}"
  }

  override def progressImpl() : IR_FieldLayout = {
    // use data type after progressing due to possible vec-mat-promotion
    val progDatatype = datatype.progress

    // determine full data dimensionality
    val numDimsData = numDimsGrid + progDatatype.dimensionality

    var layouts = Array[IR_FieldLayoutPerDim]()

    // add layouts for grid dimensions - assume 0 padding as default
    layouts ++= (0 until numDimsGrid).map(dim =>
      IR_FieldLayoutPerDim(0, ghostLayers(dim), duplicateLayers(dim), innerPoints(dim), duplicateLayers(dim), ghostLayers(dim), 0))

    // add layouts for additional dimensions introduced by the datatype - no ghost, dup, pad layers required
    if (numDimsData > numDimsGrid)
      layouts ++= progDatatype.getSizeArray.map(size => IR_FieldLayoutPerDim(0, 0, 0, size, 0, 0, 0))

    // adapt localization identifier for low-dimensional primitives - TODO: support edges directly?
    val finalDiscretization = (localization match {
      case L4_HACK_OtherLocalization(other) => L4_Localization.resolve(other.drop(5))
      case base                             => base
    }).progress

    // will be updated afterwards
    val dummyRefOffset = IR_ExpressionIndex(Array.fill(numDimsData)(0))

    val ret = IR_FieldLayout(name, level, progDatatype, finalDiscretization, layouts, numDimsGrid, dummyRefOffset,
      communicatesDuplicated, communicatesGhosts)

    // update reference offset
    ret.updateDefReferenceOffset()

    ret
  }

  override def toLayoutAccess : L4_FieldLayoutLikeAccess[IR_FieldLayout] = L4_FieldLayoutAccess(this)
}
