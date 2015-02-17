package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.logger._
import exastencils.util._
import jeremias.dsl._

trait Domain {
  def identifier : String
  def index : Int
  def shape : Any
}

case class RectangularDomain(
  var identifier : String,
  var index : Int,
  var shape : RectangularDomainShape) extends Domain {}

case class IrregularDomain(
  var identifier : String,
  var index : Int,
  var shape : IrregularDomainShape) extends Domain {}

object DomainCollection {
  var domains : ListBuffer[Domain] = ListBuffer()

  def getDomainByIdentifier(identifier : String) : Option[Domain] = {
    val ret = domains.find(d => d.identifier == identifier)
    if (ret.isEmpty) Logger.warn(s"Domain $identifier was not found")
    ret
  }

  def initFragments() {
    if (Knowledge.domain_formUnion)
      domains
        .filter { d => d.identifier == "global" || d.identifier.contains("union_") }
        .zipWithIndex
        .foreach { case (d, i) => d.shape.asInstanceOf[DomainShape].initFragments(i) }
    else {
      domains
        .find { d => d.identifier == "global" } match {
          case Some(n) => n.shape.asInstanceOf[DomainShape].initFragments(n.index)
          case None    => Logger.error("There is no domain labeled \"global\"")
        }
    }
  }
  def unifyDomains(identifier1 : String, identifier2 : String, face : Face) {
    //     
    // for now only 2D
    val domainIndex = Map(
      identifier1 -> domains.find { d => d.identifier == identifier1 }.get.index,
      identifier2 -> domains.find { d => d.identifier == identifier2 }.get.index)

    //gets the fragments located at the boundary  
    val boundaryFragments = FragmentCollection.fragments.filter { f =>
      f.vertices.exists { v =>
        (f.domainIds.contains(domainIndex(identifier1)) || f.domainIds.contains(domainIndex(identifier2))) &&
          (face.Edges.exists { e => e.contains(v) })
      }
    }
    //function to count vertices which are the same in both fragments
    val sameCoords = (x : Fragment, y : Fragment) => x.vertices.map { va => y.vertices.count { vb => vb.Coords == va.Coords } }.sum

    // mapping from globalId to the new neighborId
    val neighborIds = boundaryFragments.map { bf =>
      {
        val neighbor = boundaryFragments.filter { f => f.globalId != bf.globalId && f.domainIds != bf.domainIds }
          .maxBy { neighborFrags => (sameCoords(bf, neighborFrags)) }
        (bf.globalId -> neighbor.globalId)
      }
    }

    // lists all neighbor ids of boundary fragments, counts them and decides which negative id is the right one 
    val neighborList = ListBuffer(
      boundaryFragments.filter { bf => bf.domainIds.contains(domainIndex(identifier1)) }.flatMap { _.neighborIDs },
      boundaryFragments.filter { bf => bf.domainIds.contains(domainIndex(identifier2)) }.flatMap { _.neighborIDs })

    val nIndex = neighborList.map { nl =>
      nl.distinct.foldLeft((0, 0))((a, b) => {
        val cnt = nl.count { _ == b };
        if (cnt > a._1) (cnt, b) else a
      })._2
    }

    // updates for each fragment the corresponding neighbor id
    neighborIds.foreach {
      case (gid, nid) => {
        val n = FragmentCollection.fragments.find(f => f.globalId == gid).get.neighborIDs
        val i = (if (FragmentCollection.fragments.find(f => f.globalId == gid).get.domainIds.contains(domainIndex(identifier2))) 1 else 0)
        val r = n.indexOf(nIndex(i))
        n.update(n.indexOf(nIndex(i)), nid)
      }
    }
  }

}