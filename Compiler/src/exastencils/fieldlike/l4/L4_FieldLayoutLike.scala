package exastencils.fieldlike.l4

import exastencils.base.l4.L4_ConstIndex
import exastencils.base.l4.L4_Datatype
import exastencils.fieldlike.ir.IR_FieldLayoutLike
import exastencils.grid.l4.L4_Localization
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject

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
}
