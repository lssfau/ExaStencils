package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl


  class ImplStencil(val name: String, val matlength1 : Int,  val matlength2 : Int, val datatype: ListBuffer[String], val sizex: Int, val sizey: Int, val sizez: Int, val l: Array[Int], var ent: ListBuffer[ListBuffer[Double]], wf: Array[String], val addpoints: Int) {
    var sx: Int = sizex
    var sy: Int = sizey
    var sz: Int = sizez
    var length = l
    var weakform = wf
    var entries = ent 
    override def toString = name + " " + datatype + " " + sx + " " + sy + " " + sz + " " + length.toString
  }

