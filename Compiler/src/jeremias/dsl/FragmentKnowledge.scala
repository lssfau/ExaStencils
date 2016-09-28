package jeremias.dsl

import scala.collection.mutable._

import java.io._

import exastencils.base.ir._
import exastencils.core._
import exastencils.domain.ir.IR_DomainCollection
import exastencils.knowledge._

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
    save.write(FragmentCollection.fragments.map(f => f.toString()).mkString("\n").getBytes())

    save.close()
  }

  private def saveBin() : Unit = {

    val outData = new FragmentDataWriter(new BufferedOutputStream(new FileOutputStream(Settings.fragmentFile_config_path_binary)))
    FragmentCollection.fragments.foreach(f => {
      IR_DomainCollection.objects.foreach { d => f.binarySize += outData.writeBinary(IR_BooleanDatatype, FragmentCollection.isValidForSubDomain(f.globalId, d.index)) }
      f.binarySize += outData.writeBinary(IR_IntegerDatatype, f.globalId)
      f.binarySize += outData.writeBinary(IR_IntegerDatatype, f.localId)
      f.vertices.foreach { v => v.Coords.foreach { c => f.binarySize += outData.writeBinary(IR_RealDatatype, c) } }
      FragmentCollection.getFragPos(f.vertices).Coords.foreach { c => f.binarySize += outData.writeBinary(IR_RealDatatype, c) }

      IR_DomainCollection.objects.foreach { d => {
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
    })
    outData.close()

    val outConfig = new FragmentDataWriter(new BufferedOutputStream(new FileOutputStream(Settings.fragmentFile_config_path_domainConfig)))
    outConfig.writeInt(Knowledge.mpi_numThreads)
    for (i <- 0 until Knowledge.mpi_numThreads) {
      outConfig.writeInt(FragmentCollection.fragments.count { f => f.rank == i })
      outConfig.writeInt(FragmentCollection.fragments.filter { f => f.rank == i }.map { f => f.binarySize }.sum)
      //      println(s"rank ($i)  -  # fragments: ${FragmentCollection.fragments.count { f => f.rank == i }}, binarySize: ${FragmentCollection.fragments.filter { f => f.rank == i }.map { f => f.binarySize }.sum}")
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

  def isNeighborValid(globalId : Int, neighborId : Int, domain : Int) : Boolean = {
    fragments.find(f => f.globalId == globalId) match {
      case Some(n) => {
        n.neighborIDs.contains(neighborId) &&
          (fragments.find { nf => nf.globalId == neighborId } match {
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
          (fragments.find { nf => nf.globalId == neighborId }.get match {
            case m : Fragment => m.domainIds.contains(domain) && (getMpiRank(globalId) != getMpiRank(neighborId))
            case _            => false
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

}

class FragmentDataWriter(s : BufferedOutputStream) extends DataOutputStream(s) {
  val boolSize = 1
  val intSize = 4
  val doubleSize = 8

  def writeBinary(datatype : IR_Datatype, value : Any) : Int = {
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
