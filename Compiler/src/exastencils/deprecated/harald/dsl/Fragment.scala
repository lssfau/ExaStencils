package exastencils.deprecated.harald.dsl

import scala.collection.mutable.ListBuffer

class Fragment(f : ListBuffer[Face], e : ListBuffer[Edge], v : ListBuffer[Vertex]) {
  val faces = f
  val edges = e
  val vertices = v
}

class Primitives() {
  
}

class Vertex(c : ListBuffer[Double]) extends Primitives {
 val coords = c
 override def toString : String = {
   var s : String = "(" + coords(0).toString
   for (i <- 1 to coords.length-1)
     s += "," + coords(i)
   s += ")"  
    return s  
 }
}

class Edge(v1 : Vertex, v2 : Vertex) extends Primitives { 
  val vertex1 = v1
  val vertex2 = v2
  override def toString : String = "(" + vertex1.toString + " , " + vertex2.toString + ")" 
}

class Face(edg : ListBuffer[Edge], v : ListBuffer[Vertex]) extends Primitives { 
   val edges = edg
   val vertices = v
}