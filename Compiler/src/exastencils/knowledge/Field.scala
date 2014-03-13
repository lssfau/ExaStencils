package exastencils.knowledge

import exastencils.knowledge._
import exastencils.datastructures._

class FieldLayoutPerDim(var padBegin : Int, var ghostBegin : Int, var dupBegin : Int, var inner : Int, var dupEnd : Int, var ghostEnd : Int, var padEnd : Int) {
  def total = { padBegin + ghostBegin + dupBegin + inner + dupEnd + ghostEnd + padEnd }
}

case class Field(var identifier : String, var codeName : String, var dataType : String, var layout : Array[FieldLayoutPerDim], var level : Int, var numSlots : Int, var bcDir0 : Boolean) extends Node {

}
