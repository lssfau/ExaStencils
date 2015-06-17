package exastencils.domain

import exastencils.knowledge._
import exastencils.constraints._
import exastencils.logger._
import exastencils.spl._
import exastencils.datastructures._
import exastencils.prettyprinting._

object DomainFileHeader {
  var dimensionality = Knowledge.dimensionality

  var numBlocks = Knowledge.domain_numBlocks
  var numFragmentsPerBlock = Knowledge.domain_numFragmentsPerBlock

  var mpi_numThreads = 1
  var discr_hx : List[String] = List()
  var discr_hy : List[String] = List()
  var discr_hz : List[String] = List()

  var domainIdentifier : Array[String] = Array()
  //var domainIdentifier : String = ""

  def updateKnowledge(configuration : Configuration = new Configuration) = {
    Constraints.condEnsureValue(Knowledge.dimensionality, dimensionality, Knowledge.dimensionality != dimensionality, "using dimensionality parameter from domain file")
    Constraints.condEnsureValue(Knowledge.domain_numBlocks, numBlocks, Knowledge.domain_numBlocks != numBlocks, "using domain_numBlocks parameter from domain file")
    Constraints.condEnsureValue(Knowledge.domain_numFragmentsPerBlock, numFragmentsPerBlock, Knowledge.domain_numFragmentsPerBlock != numFragmentsPerBlock, "using domain_numFragmentsPerBlock parameter from domain file")
    Constraints.condEnsureValue(Knowledge.mpi_numThreads, mpi_numThreads, Knowledge.mpi_numThreads != mpi_numThreads, "using mpi_numThreads parameter from domain file")
    Constraints.updateValue(Knowledge.discr_hx, discr_hx.map { x => x.toDouble }.toArray)
    Constraints.updateValue(Knowledge.discr_hy, discr_hy.map { y => y.toDouble }.toArray)
    Constraints.updateValue(Knowledge.discr_hz, discr_hz.map { z => z.toDouble }.toArray)
  }

}

object DomainFileWriter extends BuildfileGenerator {
  override def write = {
    val domains = DomainCollection.domains.filter { f => f.identifier != "global" }

    val printer = PrettyprintingManager.getPrinter("DomainDefinition.cfg")

    printer <<< "#exastencils domain file"

    printer <<< "DATA"
    printer <<< "dimensionality = " + Knowledge.dimensionality
    printer <<< "numBlocks = " + Knowledge.domain_numBlocks
    printer <<< "numFragmentsPerBlock = " + Knowledge.domain_numFragmentsPerBlock
    printer <<< "mpi_numThreads = " + Knowledge.mpi_numThreads
    printer <<< "discr_hx = (" + Knowledge.discr_hx.toList.mkString(",") + ")"
    if (Knowledge.dimensionality >= 2) printer <<< "discr_hy = (" + Knowledge.discr_hy.toList.mkString(",") + ")"
    if (Knowledge.dimensionality >= 3) printer <<< "discr_hz = (" + Knowledge.discr_hz.toList.mkString(",") + ")"
    printer <<< "domainIdentifier = \"" + (domains.map { d => d.identifier }.mkString(",")) + "\""

    printer <<< "DOMAINS"
    for (d <- domains) {
      printer <<< d.identifier + " = (" + FragmentCollection.fragments.filter { f => f.rank >= 0 }
        .filter { f => f.domainIds.contains(d.index) }
        .groupBy { g => g.rank }.keySet.map { m => "b" + m.toString() }.mkString(",") + ")"
    }

    printer <<< "BLOCKS"
    for (b <- FragmentCollection.fragments.filter { f => f.rank >= 0 }.groupBy { g => g.rank }) {
      printer <<< "b" + b._1.toString + " = (" + b._2.map(m => "f" + m.globalId).mkString(",") + " )"
    }

    printer <<< "FRAGMENTS"
    for (f <- FragmentCollection.fragments.filter { f => f.rank >= 0 }) {
      printer <<< "f" + f.globalId.toString + " = " + "(" + f.faces.map {
        fa =>
          "(" + fa.Edges.map {
            e => e.toString()
          }.mkString(",") + ")"
      }.mkString(",") + ")"
    }
    printer.finish
  }
}