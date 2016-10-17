package exastencils.deprecated.domain

import scala.collection.mutable._

import java.io._

import exastencils.base.ir._
import exastencils.config._
import exastencils.domain.ir.IR_DomainCollection

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
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
    val domains = if (Knowledge.domain_readFromFile) IR_DomainCollection.getByIdentifier("global").get.shape.asInstanceOf[List[FileInputDomain]] else IR_DomainCollection.objects
    //    if (Knowledge.domain_useCase != "") {
    //      fragments = fragments.filter { f => f.domainIds.exists { e => e != 0 } }
    //    }
    fragments = fragments.sortBy { f => f.rank -> f.localId }
    fragments.foreach(f => {
      domains.foreach { d => f.binarySize += outData.writeBinary(IR_BooleanDatatype, FragmentCollection.isValidForSubDomain(f.globalId, d.index)) }
      f.binarySize += outData.writeBinary(IR_IntegerDatatype, f.globalId)
      f.binarySize += outData.writeBinary(IR_IntegerDatatype, f.localId)
      f.vertices.foreach { v => v.Coords.foreach { c => f.binarySize += outData.writeBinary(IR_RealDatatype, c) } }
      FragmentCollection.getFragPos(f.vertices).Coords.foreach { c => f.binarySize += outData.writeBinary(IR_RealDatatype, c) }
      domains.foreach { d => {
        f.neighborIDs.foreach { n => {
          val valid = FragmentCollection.isNeighborValid(f.globalId, n, d.index)
          f.binarySize += outData.writeBinary(IR_BooleanDatatype, valid)
          if (valid) {
            val remote = FragmentCollection.isNeighborRemote(f.globalId, n, d.index)
            f.binarySize += outData.writeBinary(IR_BooleanDatatype, remote)
            f.binarySize += outData.writeBinary(IR_IntegerDatatype, FragmentCollection.getLocalFragId(n))
            if (remote) {
              f.binarySize += outData.writeBinary(IR_IntegerDatatype, FragmentCollection.getMpiRank(n))
            }
          }
        }
        }
      }
      }
      val trafo = f.trafo != ListBuffer(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
      f.binarySize += outData.writeBinary(IR_BooleanDatatype, trafo)
      if (trafo) {
        f.trafo.foreach { t => f.binarySize += outData.writeBinary(IR_RealDatatype, t) }
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

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
object FragmentCollection {
  var fragments : ListBuffer[DummyFragment] = ListBuffer()

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
      case Some(n) => {
        n.neighborIDs.contains(neighborId) &&
          (fragments.find { nf => nf.globalId == neighborId && nf.rank >= 0 } match {
            case Some(m) => m.domainIds.contains(domain)
            case None    => false
          })
      }
      case None    => false
    }
  }

  def isNeighborRemote(globalId : Int, neighborId : Int, domain : Int) : Boolean = {
    fragments.find(f => f.globalId == globalId) match {
      case Some(n) => {
        n.neighborIDs.contains(neighborId) &&
          (fragments.find { nf => nf.globalId == neighborId && nf.rank >= 0 }.get match {
            case m : DummyFragment => m.domainIds.contains(domain) && (getMpiRank(globalId) != getMpiRank(neighborId))
            case _                 => false
          })
      }
      case None    => false
    }
  }

  def getFragPos(vertices : ListBuffer[Vertex]) : Vertex = {
    val position = ListBuffer(vertices(0).Coords(0) + (vertices.last.Coords(0) - vertices(0).Coords(0)) / 2.0)
    if (Knowledge.dimensionality >= 2) position += (vertices(0).Coords(1) + (vertices.last.Coords(1) - vertices(0).Coords(1)) / 2.0)
    if (Knowledge.dimensionality >= 3) position += vertices(0).Coords(2) + (vertices.last.Coords(2) - vertices(0).Coords(2)) / 2.0
    new Vertex(position)
  }

  def getNeighborIndex(fragment : DummyFragment, neighbor : DummyFragment) : Int = {
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

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
class FragmentDataWriter(s : BufferedOutputStream) extends DataOutputStream(s) {
  val boolSize = 1
  val intSize = 4
  val doubleSize = 8

  def writeBinary(datatype : IR_Datatype, value : Any) : Int = {
    //    println(t.toString() + " | " + v)
    datatype match {
      case IR_IntegerDatatype => {
        writeInt(value.asInstanceOf[Int])
        intSize
      }
      case IR_BooleanDatatype => {
        writeBoolean(value.asInstanceOf[Boolean])
        boolSize
      }
      case IR_RealDatatype    => {
        writeDouble(value.asInstanceOf[Double])
        doubleSize
      }
      case _                  => 0
    }
  }
}
