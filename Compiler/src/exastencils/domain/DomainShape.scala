package exastencils.domain

import scala.collection.mutable.ListBuffer
import exastencils.datastructures.ir._
import exastencils.util._
import scala.math._
import exastencils.knowledge._
import exastencils.util._
import exastencils.knowledge._
import exastencils.logger._

case class Interval(lower : Double, upper : Double) {
  override def toString = s"[${lower},${upper}]"
}

trait DomainShape {
  def shapeData : Any
  def contains(vertex : Vertex) : Boolean
  def initFragments() : Unit

  class Coord(x : Double, y : Double, z : Double) {
    var X : Double = x
    var Y : Double = y
    var Z : Double = z

    def /(divider : Double) : Coord = new Coord(X / divider, Y / divider, Z / divider)
    def /(divider : Int) : Coord = this / divider.toDouble
    def +(c : Coord) : Coord = new Coord(X + c.X, Y + c.Y, Z + c.Z)
    def +(v : Double) : Coord = new Coord(X + v, Y + v, Z + v)
  }

}

case class FileInputDomainShape(override val shapeData : String) extends DomainShape {

  var blocks : List[String] = List()
  var frags : List[String] = List()

  def contains(vertex : Vertex) = true //TODO
  def initFragments() = {
    val fragIds = frags.map { f => f.drop(1).toInt }
    val u = FragmentCollection.fragments
    val z = u.length
    val domainFrags = FragmentCollection.fragments.filter { f => fragIds.contains(f.globalId) }
    val nC = domainFrags.foreach { f =>
      domainFrags
        .filter { x => x.globalId != f.globalId }
        .foreach { x =>
          {
            if (Knowledge.dimensionality == 2 && f.edges.intersect(x.edges).length > 0) {
              val t = f.edges.intersect(x.edges).head
              f.faces.head.getEdgeDirection(t) match {
                case Some(d) => f.neighborIDs(d.id) = x.globalId
                case None    =>
              }
            } else if (Knowledge.dimensionality == 3) {
              //TODO
            }
          }
        }
    }
  }

}

case class LShapedDomainShape(override val shapeData : List[RectangularDomainShape]) extends DomainShape {
  def contains(vertex : Vertex) = {
    shapeData.exists { rds => rds.contains(vertex) }
  }
  def initFragments() = {
    DomainCollection.getDomainByIdentifier("global") match {
      case Some(d) => {
        d.shape.asInstanceOf[RectangularDomainShape].initFragments()
        //TODO more flexibel when several domains defined

        val frags = FragmentCollection.fragments
        val t0 = FragmentCollection.fragments.count { f => f.rank >= 11 }
        //invalid ranks (becaus of missing part in domain through L-shape) will be exchanged by valid ones
        val t1 = frags.filter { f => f.rank >= Knowledge.mpi_numThreads }.map { f => f.rank }
        val t2 = frags.filter { f => !f.domainIds.contains(1) }.map { f => f.rank }
        val rankMapping = Map(frags.filter { f => f.rank >= Knowledge.mpi_numThreads }.map { f => f.rank }
          .zip(frags.filter { f => !f.domainIds.contains(1) }.map { f => f.rank })
          .toArray : _*)
        FragmentCollection.fragments.filter { f => rankMapping.values.toList.contains(f.rank) }
          .foreach { f => f.rank = -99 }
        FragmentCollection.fragments.filter { f => rankMapping.keySet.contains(f.rank) }
          .foreach { f => f.rank = rankMapping.get(f.rank).getOrElse(f.rank) }

      }
      case None => Logger.error(s"No global Domain defined")
    }
  }

}

case class RectangularDomainShape(override val shapeData : AABB) extends DomainShape {
  var counter = -1
  val rankWidth_x : Double = (shapeData.upper_x - shapeData.lower_x) / Knowledge.domain_rect_numBlocks_x.toDouble
  val rankWidth_y : Double = (shapeData.upper_y - shapeData.lower_y) / Knowledge.domain_rect_numBlocks_y.toDouble
  val rankWidth_z : Double = (shapeData.upper_z - shapeData.lower_z) / Knowledge.domain_rect_numBlocks_z.toDouble
  val fragWidth_x : Double = rankWidth_x / Knowledge.domain_rect_numFragsPerBlock_x.toDouble
  val fragWidth_y : Double = rankWidth_y / Knowledge.domain_rect_numFragsPerBlock_y.toDouble
  val fragWidth_z : Double = rankWidth_z / Knowledge.domain_rect_numFragsPerBlock_z.toDouble

  def contains(vertex : Vertex) : Boolean = {
    vertex.Coords(0) >= shapeData.lower_x && vertex.Coords(0) <= shapeData.upper_x &&
      (if (Knowledge.dimensionality >= 2) { vertex.Coords(1) >= shapeData.lower_y && vertex.Coords(1) <= shapeData.upper_y } else true) &&
      (if (Knowledge.dimensionality >= 3) { vertex.Coords(2) >= shapeData.lower_z && vertex.Coords(2) <= shapeData.upper_z } else true)
  }
  def initFragments() = {
    //    FragmentCollection.fragments.clear()

    for (r <- 0 until Knowledge.domain_numBlocks) {
      counter = -1
      for {
        k <- 0 until Knowledge.domain_rect_numFragsPerBlock_z
        j <- 0 until Knowledge.domain_rect_numFragsPerBlock_y
        i <- 0 until Knowledge.domain_rect_numFragsPerBlock_x
      } {
        var v : ListBuffer[Vertex] = ListBuffer()
        var e : ListBuffer[Edge] = ListBuffer()
        var f : ListBuffer[Face] = ListBuffer()
        val indices = new Index(r, k, j, i)
        if (Knowledge.dimensionality == 1) {
          for (iv <- 0 to 1) {
            v += new Vertex(ListBuffer(
              getIntervalOfRank(r)("xInterval").lower + (i + iv).toDouble * fragWidth_x))
            e = ListBuffer(new Edge(v(0), v(1)))
            f = ListBuffer(new Face(ListBuffer(e(0)), v))
            val domainIDs = DomainCollection.domains.filter(dom => dom.shape.asInstanceOf[DomainShape].contains(FragmentCollection.getFragPos(v))).map(dom => dom.index)

            FragmentCollection.fragments += new Fragment(calcLocalFragmentId, calcGlobalFragmentId(indices), domainIDs, f, e, v, getNeighbours(indices), r)
          }
        } else if (Knowledge.dimensionality == 2) {
          for {
            jv <- 0 to 1
            iv <- 0 to 1
          } {
            v += new Vertex(ListBuffer(
              getIntervalOfRank(r)("xInterval").lower + (i + iv).toDouble * fragWidth_x,
              getIntervalOfRank(r)("yInterval").lower + (j + jv).toDouble * fragWidth_y))
          }
          e = ListBuffer(new Edge(v(0), v(1)), new Edge(v(0), v(2)), new Edge(v(1), v(3)), new Edge(v(2), v(3)))
          f = ListBuffer(new Face(ListBuffer(e(0), e(1), e(2), e(3)), v))
          val domainIDs = DomainCollection.domains.filter(dom => dom.shape.asInstanceOf[DomainShape].contains(FragmentCollection.getFragPos(v))).map(dom => dom.index)
          FragmentCollection.fragments += new Fragment(calcLocalFragmentId, calcGlobalFragmentId(indices), domainIDs, f, e, v, getNeighbours(indices), r)
        } else {
          // 3D
          for {
            kv <- 0 to 1
            jv <- 0 to 1
            iv <- 0 to 1
          } {
            v += new Vertex(ListBuffer(
              getIntervalOfRank(indices.r)("xInterval").lower + (indices.i + iv).toDouble * fragWidth_x,
              getIntervalOfRank(indices.r)("yInterval").lower + (indices.j + jv).toDouble * fragWidth_y,
              getIntervalOfRank(indices.r)("zInterval").lower + (indices.k + kv).toDouble * fragWidth_z))
          }

          e = ListBuffer(new Edge(v(0), v(1)), new Edge(v(0), v(2)), new Edge(v(0), v(4)), new Edge(v(1), v(3)), new Edge(v(1), v(5)), new Edge(v(2), v(3)), new Edge(v(2), v(6)),
            new Edge(v(3), v(7)), new Edge(v(4), v(5)), new Edge(v(4), v(6)), new Edge(v(5), v(7)), new Edge(v(6), v(7)))
          f = ListBuffer(new Face(ListBuffer(e(0), e(1), e(3), e(5)), ListBuffer(v(0), v(1), v(2), v(3))), //left
            new Face(ListBuffer(e(8), e(9), e(10), e(11)), ListBuffer(v(4), v(5), v(6), v(7))), //right
            new Face(ListBuffer(e(0), e(2), e(4), e(8)), ListBuffer(v(0), v(1), v(4), v(5))), //front
            new Face(ListBuffer(e(5), e(6), e(7), e(11)), ListBuffer(v(2), v(3), v(6), v(7))), //back
            new Face(ListBuffer(e(3), e(4), e(7), e(10)), ListBuffer(v(1), v(3), v(5), v(7))), //top
            new Face(ListBuffer(e(1), e(2), e(6), e(9)), ListBuffer(v(0), v(2), v(4), v(6)))) //bottom
          val domainIDs = DomainCollection.domains.filter(dom => dom.shape.asInstanceOf[DomainShape].contains(FragmentCollection.getFragPos(v))).map(dom => dom.index)
          FragmentCollection.fragments += new Fragment(calcLocalFragmentId, calcGlobalFragmentId(indices), domainIDs, f, e, v, getNeighbours(indices), r)
        }
      }
    }
  }

  def calcGlobalFragmentId(indices : Index) : Int = {
    val id = indices.getGlobalId()
    indices.checkValidity(id)
  }

  def calcLocalFragmentId() : Int = {
    //                k * Knowledge.domain_numFragsPerBlock_y * Knowledge.domain_numFragsPerBlock_x + j * Knowledge.domain_numFragsPerBlock_x + i - emptyFragFields
    counter += 1
    counter
  }

  def getNeighbours(indices : Index) : ListBuffer[Int] = {
    val n = ListBuffer[Int](calcGlobalFragmentId(indices.lowerNeighbor("i")), calcGlobalFragmentId(indices.upperNeighbor("i")))
    if (Knowledge.dimensionality >= 2) n ++= ListBuffer[Int](calcGlobalFragmentId(indices.lowerNeighbor("j")), calcGlobalFragmentId(indices.upperNeighbor("j")))
    if (Knowledge.dimensionality >= 3) n ++= ListBuffer[Int](calcGlobalFragmentId(indices.lowerNeighbor("k")), calcGlobalFragmentId(indices.upperNeighbor("k")))
    n
  }

  def getIntervalOfRank(rank : Int) : Map[String, Interval] = {
    val xFrom : Int = rank % Knowledge.domain_rect_numBlocks_x
    val yFrom : Int = (rank / Knowledge.domain_rect_numBlocks_x) % Knowledge.domain_rect_numBlocks_y
    val zFrom : Int = rank / (Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y)
    var interval = Map(
      "xInterval" -> new Interval(shapeData.lower_x + xFrom.toDouble * rankWidth_x, shapeData.lower_x + (xFrom.toDouble + 1) * rankWidth_x))
    if (Knowledge.dimensionality >= 2) interval += ("yInterval" -> new Interval(shapeData.lower_y + yFrom.toDouble * rankWidth_y, shapeData.lower_y + (yFrom.toDouble + 1) * rankWidth_y))
    if (Knowledge.dimensionality >= 3) interval += ("zInterval" -> new Interval(shapeData.lower_z + zFrom.toDouble * rankWidth_z, shapeData.lower_z + (zFrom.toDouble + 1) * rankWidth_z))
    interval
  }

  case class Index(r : Int, k : Int, j : Int, i : Int) {
    def lowerNeighbor(dir : String) : Index = {
      dir match {
        case "i" => new Index(r, k, j, i - 1)
        case "j" => new Index(r, k, j - 1, i)
        case "k" => new Index(r, k - 1, j, i)
        case _   => this
      }
    }
    def upperNeighbor(dir : String) : Index = {
      dir match {
        case "i" => new Index(r, k, j, i + 1)
        case "j" => new Index(r, k, j + 1, i)
        case "k" => new Index(r, k + 1, j, i)
        case _   => this
      }
    }

    def getGlobalId() : Int = {
      //      unionDomainIndex * Knowledge.domain_numBlocks * Knowledge.domain_numFragmentsPerBlock +
      r * Knowledge.domain_numFragmentsPerBlock +
        k * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_x +
        j * Knowledge.domain_rect_numFragsPerBlock_x +
        i
    }

    def checkValidity(id : Int) : Int = {
      //TODO improve this function!!
      val dom = DomainCollection.getDomainByIdentifier("global").get
      val size = dom.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
      if (r < 0) -7
      else if (r > Knowledge.domain_numBlocks - 1) -8

      else if (i < 0) {
        if (getIntervalOfRank(r)("xInterval").lower > 0.0) {
          val newRankId = r - 1
          calcGlobalFragmentId(new Index(newRankId, k, j, Knowledge.domain_rect_numFragsPerBlock_x - 1))
        } else -1
      } else if (j < 0) {
        if (getIntervalOfRank(r)("yInterval").lower > size.lower_y) {
          val newRankId = r - Knowledge.domain_rect_numBlocks_x
          calcGlobalFragmentId(new Index(newRankId, k, Knowledge.domain_rect_numFragsPerBlock_y - 1, i))
        } else -2
      } else if (k < 0) {
        if (getIntervalOfRank(r)("zInterval").lower > size.lower_z) {
          val newRankId = r - Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y
          calcGlobalFragmentId(new Index(newRankId, Knowledge.domain_rect_numFragsPerBlock_z - 1, j, i))
        } else -3
      } else if (i > Knowledge.domain_rect_numFragsPerBlock_x - 1) {
        val newRankId = r + 1
        if (getIntervalOfRank(newRankId)("yInterval") == getIntervalOfRank(r)("yInterval"))
          calcGlobalFragmentId(new Index(newRankId, k, j, 0))
        else -4
      } else if (j > Knowledge.domain_rect_numFragsPerBlock_y - 1) {
        val newRankId = r + Knowledge.domain_rect_numBlocks_x
        if (getIntervalOfRank(newRankId)("xInterval") == getIntervalOfRank(r)("xInterval")) {
          calcGlobalFragmentId(new Index(newRankId, k, 0, i))
        } else -5
      } else if (k > Knowledge.domain_rect_numFragsPerBlock_z - 1) {
        val newRankId = r + Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y
        if (getIntervalOfRank(newRankId)("xInterval") == getIntervalOfRank(r)("xInterval") && getIntervalOfRank(newRankId)("yInterval") == getIntervalOfRank(r)("yInterval")) {
          calcGlobalFragmentId(new Index(newRankId, 0, j, i))
        } else -6
      } else id
    }
  }

}

case class IrregularDomainShape(var faces : ListBuffer[Face], var edges : ListBuffer[Edge], var vertices : List[Vertex]) extends DomainShape {
  def shapeData = faces
  def contains(vertex : Vertex) : Boolean = {
    (vertices :+ vertices(0)).sliding(2).foldLeft(false) {
      case (c, List(i, j)) =>
        val cond = {
          ((
            (i.Coords(1) <= vertex.Coords(1) && vertex.Coords(1) < j.Coords(1)) ||
            (j.Coords(1) <= vertex.Coords(1) && vertex.Coords(1) < i.Coords(1))) &&
            (vertex.Coords(0) <= (j.Coords(0) - i.Coords(0)) * (vertex.Coords(1) - i.Coords(1)) / (j.Coords(1) - i.Coords(1)) + i.Coords(0))) //
        }
        if (cond) {
          //          println(vertex.Coords)
          !c
        } else c
    } || (vertices :+ vertices(0)).sliding(2).exists {
      case (List(i, j)) => new Edge(i, j).contains(vertex)
    }
  }
  def initFragments() : Unit = {
    FragmentCollection.fragments.clear()
    initDonutShape()
  }

  def initDonutShape() : Unit = {
    //    numberOfDomains = Knowledge.domain_ir_numBlocks
    val outerHex = getHexagonCoords(0)
    val innerHex = getHexagonCoords(0.25)
    for (r <- 0 until Knowledge.domain_numBlocks) {
      for (i <- 0 until Knowledge.domain_numFragmentsPerBlock) {
        var v : ListBuffer[Vertex] = ListBuffer()
        var e : ListBuffer[Edge] = ListBuffer()
        var f : ListBuffer[Face] = ListBuffer()
        val id = r * Knowledge.domain_numFragmentsPerBlock + i
        for (jv <- 0 to 1) {
          v += new Vertex(ListBuffer(outerHex(id + jv).X, outerHex(id + jv).Y))
          v += new Vertex(ListBuffer(innerHex(id + jv).X, innerHex(id + jv).Y))
        }
        e = ListBuffer(new Edge(v(0), v(1)), new Edge(v(0), v(2)), new Edge(v(1), v(3)), new Edge(v(2), v(3)))
        f = ListBuffer(new Face(ListBuffer(e(0), e(1), e(2), e(3)), v))
        val domainIds = DomainCollection.domains.filter(dom => dom.shape.asInstanceOf[DomainShape].contains(FragmentCollection.getFragPos(v))).map(dom => dom.index)
        FragmentCollection.fragments += new Fragment(i, id, domainIds, f, e, v, getNeighbours(id), r)

      }
    }

  }

  //TODO get PolygonCoords
  def getHexagonCoords(borderDistance : Double) : ListBuffer[Coord] = {
    val coords = ListBuffer(new Coord(-1 + borderDistance, 0, 0))
    coords += new Coord(math.sin(30 * (math.Pi / 180)) * coords(0).X, -math.cos(30 * (math.Pi / 180)) * coords(0).X, 0)
    coords += new Coord(-coords(1).X, coords(1).Y, 0)
    coords += new Coord(-coords(0).X, 0, 0)
    coords += new Coord(-coords(1).X, -coords(1).Y, 0)
    coords += new Coord(coords(1).X, -coords(1).Y, 0)
    coords += coords(0) //makes it easier to loop
    coords.map { c => (c + 1.0) / 2.0 } // normalize it to a domain from 0 to 1 in each direction
  }

  def getNeighbours(id : Int) : ListBuffer[Int] = {
    val n = ListBuffer[Int](if (id - 1 >= 0) id - 1 else Knowledge.domain_numFragmentsTotal - 1, if (id + 1 < Knowledge.domain_numFragmentsTotal) id + 1 else 0)
    n ++= ListBuffer[Int](-id * 2 - 1, -id * 2 - 2)
    n
  }
}

class Donut(var f : ListBuffer[Face], var e : ListBuffer[Edge], var v : List[Vertex]) extends IrregularDomainShape(f, e, v) {

}
