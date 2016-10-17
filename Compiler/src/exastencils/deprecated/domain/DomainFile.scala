package exastencils.deprecated.domain

import exastencils.config._
import exastencils.constraints._
import exastencils.domain.ir.IR_DomainCollection
import exastencils.prettyprinting._

/// DomainFileHeader

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
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

  def updateKnowledge() = {
    Constraints.condEnsureValue(Knowledge.dimensionality, dimensionality, Knowledge.dimensionality != dimensionality, "using dimensionality parameter from domain file")
    Constraints.condEnsureValue(Knowledge.domain_numBlocks, numBlocks, Knowledge.domain_numBlocks != numBlocks, "using domain_numBlocks parameter from domain file")
    Constraints.condEnsureValue(Knowledge.domain_numFragmentsPerBlock, numFragmentsPerBlock, Knowledge.domain_numFragmentsPerBlock != numFragmentsPerBlock, "using domain_numFragmentsPerBlock parameter from domain file")
    Constraints.condEnsureValue(Knowledge.mpi_numThreads, mpi_numThreads, Knowledge.mpi_numThreads != mpi_numThreads, "using mpi_numThreads parameter from domain file")
    Constraints.updateValue(Knowledge.discr_hx, discr_hx.map { x => x.toDouble }.toArray)
    Constraints.updateValue(Knowledge.discr_hy, discr_hy.map { y => y.toDouble }.toArray)
    Constraints.updateValue(Knowledge.discr_hz, discr_hz.map { z => z.toDouble }.toArray)
  }

}

/// DomainFileWriter

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
object DomainFileWriter extends BuildfileGenerator {
  override def write() = {
    val domains = if (Knowledge.domain_useCase != "") IR_DomainCollection.objects.filter { f => f.identifier != "global" } else IR_DomainCollection.objects
    val fragments = if (Knowledge.domain_useCase != "") FragmentCollection.fragments.filter { f => f.domainIds.exists { d => d != 0 } } else FragmentCollection.fragments

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
    printer <<< "domainIdentifier = \"" + domains.map { d => d.identifier }.mkString(",") + "\""

    printer <<< "DOMAINS"
    for (d <- domains) {
      printer <<< d.identifier + " = (" + fragments.filter { f => f.rank >= 0 }
        .filter { f => f.domainIds.contains(d.index) }
        .groupBy { g => g.rank }.keySet.map { m => "b" + m.toString() }.mkString(",") + ")"
    }

    printer <<< "BLOCKS"
    for (b <- fragments.filter { f => f.rank >= 0 }.groupBy { g => g.rank }) {
      printer <<< "b" + b._1.toString + " = (" + b._2.map(m => "f" + m.globalId).mkString(",") + " )"
    }

    printer <<< "FRAGMENTS"
    for (f <- fragments.filter { f => f.rank >= 0 }) {
      printer <<< "f" + f.globalId.toString + " = " + "(" + f.faces.map {
        fa =>
          "(" + fa.Edges.map {
            e => e.toString()
          }.mkString(",") + ")"
      }.mkString(",") + ")"
    }

    printer <<< "TRAFOS"
    for (f <- fragments.filter { f => f.rank >= 0 }) {
      val it = f.trafo.iterator
      printer << "f" + f.globalId.toString + " = " + "("
      var tmp = ""
      for (i <- 0 to 3) {
        tmp += "(" + List(it.next().toString, it.next().toString, it.next().toString, it.next().toString).mkString(",") + "),"
      }
      printer << tmp.dropRight(1)
      printer <<< ")"

    }

    printer.finish()
  }
}
