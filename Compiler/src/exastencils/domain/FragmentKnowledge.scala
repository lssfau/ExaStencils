package exastencils.domain
import scala.collection.mutable._
import java.io.{ FileDescriptor, BufferedOutputStream, DataOutputStream, FileOutputStream }
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.datastructures.ir.iv._
import exastencils.core._

object FragmentKnowledge {

  def saveFragmentData() : Unit = {
    Settings.fragmentFile_config_output match {
      case 0 => {
        saveBin()
      }
      case 1 => saveDef()
      case 2 => {
        saveBin()
        saveDef()
      }
      case _ => {}
    }
  }

  private def saveDef() = {
    val save : FileOutputStream = new FileOutputStream(Settings.fragmentFile_config_path_readable)
    if (Knowledge.domain_useCase != "") {
      val t = FragmentCollection.fragments.filter { f => f.domainIds.exists { e => e != 0 } }
      save.write(FragmentCollection.fragments.filter { f => f.domainIds.exists { e => e != 0 } }.map(f => f.toString()).mkString("\n").getBytes())
    } else {
      save.write(FragmentCollection.fragments.map(f => f.toString()).mkString("\n").getBytes())
    }

    save.close()
  }

  private def saveBin() : Unit = {

    val outData = new FragmentDataWriter(new BufferedOutputStream(new FileOutputStream(Settings.fragmentFile_config_path_binary)))
    var fragments = FragmentCollection.fragments
    val domains = if (Knowledge.domain_readFromFile) DomainCollection.getDomainByIdentifier("global").get.shape.asInstanceOf[List[FileInputDomain]] else DomainCollection.domains
    //    if (Knowledge.domain_useCase != "") {
    //      fragments = fragments.filter { f => f.domainIds.exists { e => e != 0 } }
    //    }
    fragments = fragments.sortBy { f => f.rank -> f.localId }
    fragments.foreach(f => {
      domains.foreach { d => f.binarySize += outData.writeBinary(BooleanDatatype, FragmentCollection.isValidForSubDomain(f.globalId, d.index)) }
      f.binarySize += outData.writeBinary(IntegerDatatype, f.globalId)
      f.binarySize += outData.writeBinary(IntegerDatatype, f.localId)
      f.vertices.foreach { v => v.Coords.foreach { c => f.binarySize += outData.writeBinary(RealDatatype, c) } }
      FragmentCollection.getFragPos(f.vertices).Coords.foreach { c => f.binarySize += outData.writeBinary(RealDatatype, c) }
      domains.foreach { d =>
        {
          f.neighborIDs.foreach { n =>
            {
              val valid = FragmentCollection.isNeighborValid(f.globalId, n, d.index)
              f.binarySize += outData.writeBinary(BooleanDatatype, valid)
              if (valid) {
                val remote = FragmentCollection.isNeighborRemote(f.globalId, n, d.index)
                f.binarySize += outData.writeBinary(BooleanDatatype, remote)
                f.binarySize += outData.writeBinary(IntegerDatatype, FragmentCollection.getLocalFragId(n))
                if (remote) {
                  f.binarySize += outData.writeBinary(IntegerDatatype, FragmentCollection.getMpiRank(n))
                }
              }
            }
          }
        }
      }
      val trafo = f.trafo != ListBuffer(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
      f.binarySize += outData.writeBinary(BooleanDatatype, trafo)
      if (trafo) {
        f.trafo.foreach { t => f.binarySize += outData.writeBinary(RealDatatype, t) }
      }

    })
    outData.close()

    val outConfig = new FragmentDataWriter(new BufferedOutputStream(new FileOutputStream(Settings.fragmentFile_config_path_domainConfig)))
    outConfig.writeInt(Knowledge.mpi_numThreads)
    val ranks = fragments.map { f => f.rank }.toSet.toList.sorted
    for (i <- ranks) {
      //outConfig.writeInt(i)
      outConfig.writeInt(fragments.count { f => f.rank == i })
      outConfig.writeInt(fragments.filter { f => f.rank == i }.map { f => f.binarySize }.sum)
      //println(s"rank ($i)  -  # fragments: ${FragmentCollection.fragments.count { f => f.rank == i }}, binarySize: ${FragmentCollection.fragments.filter { f => f.rank == i }.map { f => f.binarySize }.sum}")
    }
    outConfig.close()
  }

}

object FragmentCollection {
  var fragments : ListBuffer[Fragment] = ListBuffer()

  def getLocalFragId(globalId : Int) : Int = {
    fragments.find(f => f.globalId == globalId) match {
      case Some(n) => n.localId
      case None    => globalId
    }
  }
  def getMpiRank(globalId : Int) : Int = {
    fragments.find(f => f.globalId == globalId) match {
      case Some(n) => n.rank
      case None    => globalId
    }
  }

  def getDomainIds(globalId : Int) : ListBuffer[Int] = {
    fragments.find(f => f.globalId == globalId) match {
      case Some(n) => n.domainIds
      case None    => ListBuffer()
    }
  }

  def getRemoteRank(globalId : Int, domainId : Int) : Int = {
    fragments.find(f => f.globalId == globalId) match {
      case Some(n) => n.rank
      case None    => -1
    }
  }

  def isValidForSubDomain(globalId : Int, domain : Int) : Boolean = {
    fragments.find(f => f.globalId == globalId) match {
      case Some(n) => n.domainIds.contains(domain)
      case None    => false
    }
  }

  def getNumberOfNeighbors() : Int = {
    if (Knowledge.comm_strategyFragment == 26) {
      Knowledge.dimensionality match {
        case 1 => 2
        case 2 => 8
        case 3 => 26
        case _ => 0
      }
    } else Knowledge.dimensionality * 2
  }

  def isNeighborValid(globalId : Int, neighborId : Int, domain : Int) : Boolean = {
    fragments.find(f => f.globalId == globalId) match {
      case Some(n) =>
        {
          n.neighborIDs.contains(neighborId) &&
            (fragments.find { nf => nf.globalId == neighborId && nf.rank >= 0 } match {
              case Some(m) => m.domainIds.contains(domain)
              case None    => false
            })
        }
      case None => false
    }
  }

  def isNeighborRemote(globalId : Int, neighborId : Int, domain : Int) : Boolean = {
    fragments.find(f => f.globalId == globalId) match {
      case Some(n) =>
        {
          n.neighborIDs.contains(neighborId) &&
            (fragments.find { nf => nf.globalId == neighborId && nf.rank >= 0 }.get match {
              case m : Fragment => m.domainIds.contains(domain) && (getMpiRank(globalId) != getMpiRank(neighborId))
              case _            => false
            })
        }
      case None => false
    }
  }

  def getFragPos(vertices : ListBuffer[Vertex]) : Vertex = {
    val position = ListBuffer(vertices(0).Coords(0) + (vertices.last.Coords(0) - vertices(0).Coords(0)) / 2.0)
    if (Knowledge.dimensionality >= 2) position += (vertices(0).Coords(1) + (vertices.last.Coords(1) - vertices(0).Coords(1)) / 2.0)
    if (Knowledge.dimensionality >= 3) position += vertices(0).Coords(2) + (vertices.last.Coords(2) - vertices(0).Coords(2)) / 2.0
    new Vertex(position)
  }

  def getNeighborIndex(fragment : Fragment, neighbor : Fragment) : Int = {
    val fragPos = getFragPos(fragment.vertices)
    val neiPos = getFragPos(neighbor.vertices)
    val i = (if (neiPos.Coords(0) < fragPos.Coords(0)) -1 else if (neiPos.Coords(0) > fragPos.Coords(0)) 1 else 0)
    val j = (if (Knowledge.dimensionality >= 2) { (if (neiPos.Coords(1) < fragPos.Coords(1)) -1 else if (neiPos.Coords(1) > fragPos.Coords(1)) 1 else 0) } else 0)
    val k = (if (Knowledge.dimensionality >= 3) { (if (neiPos.Coords(2) < fragPos.Coords(2)) -1 else if (neiPos.Coords(2) > fragPos.Coords(2)) 1 else 0) } else 0)

    var index = 0
    var value = 0
    for (
      kStep <- (if (Knowledge.dimensionality > 2) (-1 to 1) else (0 to 0));
      jStep <- (if (Knowledge.dimensionality > 1) (-1 to 1) else (0 to 0));
      iStep <- -1 to 1;
      if (0 != kStep || 0 != jStep || 0 != iStep)
    ) {
      if (kStep == k && jStep == j && iStep == i) value = index
      index += 1
    }
    value
  }

}

class FragmentDataWriter(s : BufferedOutputStream) extends DataOutputStream(s) {
  val boolSize = 1
  val intSize = 4
  val doubleSize = 8

  def writeBinary(t : Datatype, v : Any) : Int = {
    //    println(t.toString() + " | " + v)
    t match {
      case IntegerDatatype => {
        writeInt(v.asInstanceOf[Int])
        intSize
      }
      case BooleanDatatype => {
        writeBoolean(v.asInstanceOf[Boolean])
        boolSize
      }
      case RealDatatype => {
        writeDouble(v.asInstanceOf[Double])
        doubleSize
      }
      case _ => 0
    }
  }
}
