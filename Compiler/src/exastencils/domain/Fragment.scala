package exastencils.domain

import exastencils.knowledge._

import scala.collection.mutable._

class Fragment(lId : Int, gId : Int, dId : ListBuffer[Int], f : ListBuffer[Face], e : ListBuffer[Edge], v : ListBuffer[Vertex], n : ListBuffer[Int], r : Int) {
  import Direction._
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
    if (neighborIDs.nonEmpty) {
      str += "\tNeighbours : [\n"
      if (Knowledge.comm_strategyFragment == 26) {
        neighborIDs.zipWithIndex.foreach { case (n, i) => str += s"\t\tn$i : $n" + " \n" }

      } else {
        str += s"\t\tleft: ${neighborIDs(Direction.Left.id)}" + " \n"
        str += s"\t\tright:${neighborIDs(Direction.Right.id)}" + " \n"
        if (Knowledge.dimensionality >= 2) {
          str += s"\t\tbottom:${neighborIDs(Direction.Bottom.id)}" + " \n"
          str += s"\t\ttop:${neighborIDs(Direction.Top.id)}" + " \n"
        }
        if (Knowledge.dimensionality >= 3) {
          str += s"\t\tfront:${neighborIDs(Direction.Front.id)}" + " \n"
          str += s"\t\tback:${neighborIDs(Direction.Back.id)}" + " \n"
        }
      }
      str += "\t]\n"
    }
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
    var s = "("
    for (i <- 0 to coords.size - 1) {
      s += "%.5f".format(coords(i).toFloat) + ","
    }
    s = s.dropRight(1).toString + ")"
    s
  }
  override def equals(that : Any) : Boolean = {
    that match {
      case o : Vertex => {
        o.Coords(0) == o.Coords(0) &&
          (if (Knowledge.dimensionality >= 2) o.Coords(1) == Coords(1) else true) &&
          (if (Knowledge.dimensionality >= 3) o.Coords(2) == Coords(2) else true)

      }
      case _ => false
    }
  }
  override def hashCode() = {
    41 * Coords.zipWithIndex.map { x => 41 * x._1.hashCode() + x._2.hashCode() }.sum
  }
}

class Edge(v1 : Vertex, v2 : Vertex) extends Primitives {
  val vertex1 = v1
  val vertex2 = v2
  override def toString : String = "(" + vertex1.toString + " , " + vertex2.toString + ")"

  override def equals(that : Any) = {
    that match {
      case o : Edge => {
        val v1 = vertex1 == o.vertex1
        val v2 = vertex2 == o.vertex2
        (vertex1 == o.vertex1 && vertex2 == o.vertex2) ||
          (vertex2 == o.vertex1 && vertex1 == o.vertex2)
      }
      case _ => false
    }
  }

  override def hashCode = {
    vertex1.Coords.map { x => x.hashCode() }.sum.hashCode() + vertex2.Coords.map { x => x.hashCode() }.sum.hashCode()
  }

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
      tmp
    } else { //this means one vertical line
      v.Coords(0) == vertex1.Coords(0) &&
        v.Coords(1) <= math.max(vertex1.Coords(1), vertex2.Coords(1)) &&
        v.Coords(1) >= math.min(vertex1.Coords(1), vertex2.Coords(1))
    }
  }

}

class Face(edges : ListBuffer[Edge], vertices : ListBuffer[Vertex]) extends Primitives {
  import Direction._
  val Edges = edges
  val Vertices = vertices

  override def equals(that : Any) = {
    that match {
      case o : Face => Edges.intersect(o.Edges).length == Edges.length
      case _        => false
    }

  }

  def getEdgeDirection(edge : Edge) : Option[Direction] = {

    if (edges.contains(edge)) {

      if (Knowledge.dimensionality == 2) {
        if (edge.vertex1.Coords(0) == edge.vertex2.Coords(0)) //y alligned
          if (edge.vertex1.Coords(0) == vertices.map { x => x.Coords(0) }.min) Some(Direction.Left) else Some(Direction.Right)
        else if (edge.vertex1.Coords(1) == edge.vertex2.Coords(1)) //x alligned
          if (edge.vertex1.Coords(1) == vertices.map { x => x.Coords(1) }.min) Some(Direction.Bottom) else Some(Direction.Top)
        else None
      } else None //TODO behaviour in 3D
    } else {
      None
    }
  }

  def contains(e : Edge) : Boolean = {
    //TODO face contains edge
    true
  }

  def contains(v : Vertex) : Boolean = {
    //TODO face contains vertex
    true
  }
}

object Direction extends Enumeration {
  type Direction = Value
  val Left, Right, Bottom, Top, Front, Back = Value
}
