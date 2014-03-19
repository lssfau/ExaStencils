package exastencils.knowledge

import exastencils.knowledge._
import exastencils.datastructures._

class FieldLayoutPerDim(var numPadLayersLeft : Int, var numGhostLayersLeft : Int, var numDupLayersLeft : Int,
    var numInnerLayers : Int,
    var numDupLayersRight : Int, var numGhostLayersRight : Int, var numPadLayersRight : Int) {
  def idxPadLeftBegin = { 0 }
  def idxPadLeftEnd = { idxPadLeftBegin + numPadLayersLeft - 1}
  
  def idxGhostLeftBegin = { idxPadLeftBegin + numPadLayersLeft }
  def idxGhostLeftEnd = { idxGhostLeftBegin + numGhostLayersLeft - 1}
  
  def idxDupLeftBegin = { idxGhostLeftBegin + numGhostLayersLeft }
  def idxDupLeftEnd = { idxDupLeftBegin + numDupLayersLeft - 1}
 
  def idxInnerBegin = { idxDupLeftBegin + numDupLayersLeft }
  def idxInnerEnd = { idxInnerBegin + numInnerLayers - 1 }

  def idxDupRightBegin = { idxInnerBegin + numInnerLayers }
  def idxDupRightEnd = { idxDupRightBegin + numDupLayersRight - 1 }
  
  def idxGhostRightBegin = { idxDupRightBegin + numDupLayersRight }
  def idxGhostRightEnd = { idxGhostRightBegin + numGhostLayersRight - 1 }
  
  def idxPadRightBegin = { idxGhostRightBegin + numGhostLayersRight }
  def idxPadRightEnd = { idxPadRightBegin + numPadLayersRight - 1 }
  
  def total = { idxPadRightBegin + numPadLayersRight }
}

case class Field(var identifier : String, var codeName : String, var dataType : String, var layout : Array[FieldLayoutPerDim], var level : Int, var numSlots : Int, var bcDir0 : Boolean) extends Node {

}
