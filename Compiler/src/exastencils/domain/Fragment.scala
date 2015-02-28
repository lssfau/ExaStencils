package exastencils.domain

import exastencils.knowledge._

import scala.collection.mutable._

class Fragment(lId : Int, gId : Int, dId : ListBuffer[Int], f : ListBuffer[Face], e : ListBuffer[Edge], v : ListBuffer[Vertex], n : ListBuffer[Int], r : Int) {
  val faces = f
  val edges = e
  val vertices = v
  val globalId = gId
  val localId = lId
  var neighborIDs = n
  val domainIds = dId

  var binarySize = 0
  var rank = r

  def getBinarySize() : Int = {
    binarySize
  }

  override def toString : String = {

    var str = s"Fragment : [\n"
    str += s"\tdomainIds : ${domainIds.mkString(",")}\n"
    str += s"\tglobalId : $globalId\n"
    str += s"\tlocalId : $localId\n"
    if (Knowledge.mpi_enabled) str += s"\tmpiRank : $rank\n"
    str += s"\tVertices : [\n"
    vertices.foreach(v => {
      str += s"\t\t(" + v.toString + ")\n"
    })
    str += "\t]\n"
    str += "\tPosition : [\n"
    str += s"\t\t(${vertices(0).Coords(0) + (vertices.last.Coords(0) - vertices(0).Coords(0)) / 2.0}"
    if (Knowledge.dimensionality == 2) str += s",${vertices(0).Coords(1) + (vertices.last.Coords(1) - vertices(0).Coords(1)) / 2.0}"
    if (Knowledge.dimensionality == 3) str += s",${vertices(0).Coords(2) + (vertices.last.Coords(2) - vertices(0).Coords(2)) / 2.0}"
    str += ")\n"
    str += "\t]\n"
    str += "\tNeighbours : [\n"
    str += s"\t\tleft: ${neighborIDs(0)}" + " \n"
    str += s"\t\tright:${neighborIDs(1)}" + " \n"
    if (Knowledge.dimensionality >= 2) {
      str += s"\t\tbottom:${neighborIDs(2)}" + " \n"
      str += s"\t\ttop:${neighborIDs(3)}" + " \n"
    }
    if (Knowledge.dimensionality >= 3) {
      str += s"\t\tfront:${neighborIDs(4)}" + " \n"
      str += s"\t\tback:${neighborIDs(5)}" + " \n"
    }
    str += "\t]\n"
    str += "]\n"
    str
  }
}

class Primitives() {
  val eta = 0.0005
}

trait Coord {
  var X : Double
  var Y : Double
  var Z : Double
}

class Vertex(coords : ListBuffer[Double]) extends Primitives {
  val Coords = coords
  override def toString : String = {
    var s = ""
    for (i <- 0 to coords.size - 1) {
      s += "%.5f".format(coords(i).toFloat) + ","
    }
    s = s.dropRight(1).toString
    s
  }
  override def equals(that : Any) : Boolean = {
    that match {
      case that : Vertex => {
        that.Coords(0) == Coords(0) &&
          (if (Knowledge.dimensionality >= 2) that.Coords(1) == Coords(1) else true) &&
          (if (Knowledge.dimensionality >= 3) that.Coords(2) == Coords(2) else true)
      }
      case _ => false
    }
  }
}

class Edge(vertex1 : Vertex, vertex2 : Vertex) extends Primitives {
  override def toString : String = "(" + vertex1.toString + " , " + vertex2.toString + ")"

  def contains(v : Vertex) : Boolean = {
    if ((vertex2.Coords(0) - vertex1.Coords(0)) != 0) {
      val m = (vertex2.Coords(1) - vertex1.Coords(1)) / (vertex2.Coords(0) - vertex1.Coords(0))
      val t = vertex1.Coords(1) - m * vertex1.Coords(0)
      val yS = m * v.Coords(0) + t
      val tmp = (math.abs(yS - v.Coords(1)) <= eta) &&
        v.Coords(0) <= math.max(vertex1.Coords(0), vertex2.Coords(0)) &&
        v.Coords(0) >= math.min(vertex1.Coords(0), vertex2.Coords(0)) &&
        v.Coords(1) <= math.max(vertex1.Coords(1), vertex2.Coords(1)) &&
        v.Coords(1) >= math.min(vertex1.Coords(1), vertex2.Coords(1))
      if (tmp) {

        val sewe = 2
      }
      tmp
    } else { //this means one vertical line
      v.Coords(0) == vertex1.Coords(0) &&
        v.Coords(1) <= math.max(vertex1.Coords(1), vertex2.Coords(1)) &&
        v.Coords(1) >= math.min(vertex1.Coords(1), vertex2.Coords(1))
    }
  }

}

class Face(edges : ListBuffer[Edge], vertices : ListBuffer[Vertex]) extends Primitives {
  val Edges = edges
  val Vertices = vertices
  def contains(e : Edge) : Boolean = {
    //TODO face contains edge
    true
  }

  def contains(v : Vertex) : Boolean = {
    //TODO face contains vertex
    true
  }
}
