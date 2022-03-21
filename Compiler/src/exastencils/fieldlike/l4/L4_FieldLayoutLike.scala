package exastencils.fieldlike.l4

import exastencils.base.l4.L4_ConstIndex
import exastencils.base.l4.L4_Datatype
import exastencils.config.Knowledge
import exastencils.fieldlike.ir.IR_FieldLayoutLike
import exastencils.grid.l4._
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject


object L4_FieldLayoutLike {
    def default_ghostLayers(localization : L4_Localization) : L4_ConstIndex = {
        L4_ConstIndex(Array.fill(Knowledge.dimensionality)(0))
    }

    def default_duplicateLayers(localization : L4_Localization) : L4_ConstIndex = {
        localization match {
            case L4_AtNode       => L4_ConstIndex(Array.fill(Knowledge.dimensionality)(1))
            case L4_AtCellCenter => L4_ConstIndex(Array.fill(Knowledge.dimensionality)(0))

            case L4_AtFaceCenter(faceDim) =>
                val numLayers = L4_ConstIndex(Array.fill(Knowledge.dimensionality)(0))
                numLayers(faceDim) = 1
                numLayers

            case L4_HACK_OtherLocalization("edge_node") =>
                val numLayers = L4_ConstIndex(Array.fill(Knowledge.dimensionality)(0))
                numLayers(0) = 1
                numLayers

            case L4_HACK_OtherLocalization("edge_cell") => L4_ConstIndex(Array.fill(Knowledge.dimensionality)(0))
        }
    }

    def getDefaultValue(optionName : String, localization : L4_Localization) : L4_ConstIndex = {
        optionName match {
            case "ghostLayers"     => default_ghostLayers(localization)
            case "duplicateLayers" => default_duplicateLayers(localization)
        }
    }

    def getDefaultBoolean(optionName : String, localization : L4_Localization) : Boolean = {
        optionName match {
            case "ghostLayers"     => false // no communication by default
            case "duplicateLayers" => false // no communication by default
        }
    }
}

trait L4_FieldLayoutLike[IR_FieldLayoutAbstraction <: IR_FieldLayoutLike] extends L4_LeveledKnowledgeObject[IR_FieldLayoutAbstraction] {
    def name : String // will be used to find the layout
    def level : Int // the level the field lives on
    def numDimsGrid : Int // the number of dimensions of the grid
    def datatype : L4_Datatype
    def localization : L4_Localization
    def ghostLayers : L4_ConstIndex
    def communicatesGhosts : Boolean
    def duplicateLayers : L4_ConstIndex
    def communicatesDuplicated : Boolean
    def innerPoints : L4_ConstIndex

    def toLayoutAccess : L4_FieldLayoutLikeAccess[IR_FieldLayoutAbstraction]
}
