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

package exastencils.field.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.base.l4.L4_ConstIndex
import exastencils.config.Knowledge
import exastencils.field.l4._
import exastencils.fieldlike.l3.L3_FieldLikeCollection
import exastencils.fieldlike.l3.L3_FieldLikeCollections
import exastencils.fieldlike.l4.L4_FieldLikeLayout
import exastencils.grid.l3.L3_Localization
import exastencils.knowledge.l3._
import exastencils.scheduling.NoStrategyWrapper

/// L3_FieldCollection

object L3_FieldCollection extends L3_FieldLikeCollection[L3_Field, L4_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_FieldLikeCollections.register(this)
  L3_KnowledgeContainer.register(this)

  override def name = "L3_FieldCollection"
  override def progress() = {
    prepareFieldLayouts()
    objects.foreach(obj => L4_FieldCollection.add(obj.progress()))
  }

  def prepareFieldLayouts() = {
    var requiredLayouts = HashMap[(String, Int), (L3_Datatype, L3_Localization)]()
    for (field <- objects)
      requiredLayouts += ((field.fieldLayoutName, field.level) -> (field.datatype, field.localization))

    def defIndex = L4_ConstIndex(Array.fill(Knowledge.dimensionality - 1)(0))

    for (layout <- requiredLayouts.toList.sortBy(_._1._2).sortBy(_._1._1)) {
      // generate layout
      val genLayout = L4_FieldLayout(
        layout._1._1, // name
        layout._1._2, // level
        Knowledge.dimensionality, // dims
        layout._2._1.progress, // datatype
        layout._2._2.progress, // localization
        L4_FieldLikeLayout.default_ghostLayers(layout._2._2.progress), // to be determined later
        false,
        L4_FieldLikeLayout.default_duplicateLayers(layout._2._2.progress),
        false,
        defIndex)

      // register layout
      L4_FieldLayoutCollection.add(genLayout)
    }
  }
}

/// L3_AddInitFieldsFunctionWrapper

object L3_AddInitFieldsFunctionWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => L3_FieldCollection.addInitFieldsFunction()
}
