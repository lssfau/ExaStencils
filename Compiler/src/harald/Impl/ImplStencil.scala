package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl._


  class ImplStencil(val name: String, val part : String, val datatype: String, val sizex: Int, val sizey: Int, val sizez: Int, val l: Int, val entries: Array[Double], wf: String, val addpoints: Int) {
    var sx: Int = sizex
    var sy: Int = sizey
    var sz: Int = sizez
    var length = l
    var weakform = wf
    override def toString = name + " " + datatype + " " + sx + " " + sy + " " + sz + " " + length
  }

