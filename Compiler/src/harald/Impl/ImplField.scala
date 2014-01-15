package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl._

  class ImplField(val name: String, val datatype: String, val veclength : Int,  val arrname : String, val sizex: Int, val sizey: Int, val sizez: Int, val addpoints: Int, val ghostlayers : Int)  {
    var sx: Int = sizex
    var sy: Int = sizey
    var sz: Int = sizez
    override def toString = name + " " + datatype + " " + arrname + " " + sx + " " + sy + " " + sz + " " + addpoints
  }
