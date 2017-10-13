package devel


import scala.collection.mutable.ArrayBuffer
import scala.io.Source

//import org.exastencils.schedopt.exploration.DomainCoeffInfo
//import org.exastencils.schedopt.exploration.Exploration
//import org.exastencils.schedopt.exploration.PartialSchedule

import exastencils.polyhedron.Isl

case class ExplConfig(id : Int, bands : Array[Int], schedule : isl.UnionMap, schedVects : ArrayBuffer[Array[Int]])

object AnalyzeExpl {

  private val explConfigsFile : String = "T:/work/poly_expl/jacobi_vc/ExaExpl/expl_config.txt"

  private def loadConfigs(path : String) : (isl.UnionSet, isl.UnionMap, Iterator[ExplConfig]) = {
    var lines : Iterator[String] = Source.fromFile(path).getLines()
    // first three lines are domain, dependencies and a flag for extended exploration...
    val domain = isl.UnionSet.readFromStr(Isl.ctx, lines.next())
    val deps = isl.UnionMap.readFromStr(Isl.ctx, lines.next())
    val extExpl = java.lang.Boolean.parseBoolean(lines.next())
    // .. followed by an empty line
    lines.next()

    val configs = new Iterator[ExplConfig]() {
      override def hasNext() : Boolean = {
        return lines.hasNext
      }
      override def next() : ExplConfig = {
        val Array(idStr, bandsStr, scheduleStr, vectorStr) = lines.next().split("\t")
        val id : Int = Integer.parseInt(idStr)
        val bands : Array[Int] = bandsStr.split(",").map(str => Integer.parseInt(str))
        val schedule : isl.UnionMap = isl.UnionMap.readFromStr(Isl.ctx, scheduleStr)
        val schedVects : ArrayBuffer[Array[Int]] =
          vectorStr.split("[\\[\\]](, )?").view.filterNot(_.isEmpty()).map {
            v => v.split(", ").map(Integer.parseInt)
          }.to[ArrayBuffer]
        return ExplConfig(id, bands, schedule, schedVects)
      }
    }

    return (domain, deps, configs)
  }

  def main(args : Array[String]) : Unit = {
//    val (domain, depsUm, configs) : (isl.UnionSet, isl.UnionMap, Iterator[ExplConfig]) = loadConfigs(explConfigsFile)
//    val deps : ArrayBuffer[isl.BasicMap] = Exploration.preprocess(depsUm)
//    val domInfo = DomainCoeffInfo(domain)
//
//    val df = new DecimalFormat()
//    df.setMinimumIntegerDigits(2)
//    println("Dependences:")
//    val depId = new HashMap[isl.BasicMap, String]()
//    for ((d, i) <- deps.view.zipWithIndex) {
//      val is = df.format(i)
//      depId(d) = is
//      println(s"  ${is}:  ${d.asMap()}")
//    }
//    println()
//
//    for (ExplConfig(id, bands, schedule, schedVecs) <- configs) {
//      var remDeps = deps.clone()
//      val partSched = new PartialSchedule(domInfo, remDeps)
//      var sbNr = new StringBuilder()
//      var sbDL = new StringBuilder()
//      for (schedCoeffs <- schedVecs) {
//        partSched.addScheduleVector(schedCoeffs)
//        remDeps = partSched.filterRemainingDeps(remDeps)
//        sbNr.append(remDeps.length).append("; ")
//        sbDL.append(remDeps.view.map(depId).mkString(",")).append("; ")
//      }
//      print(id)
//      print("; ")
//      print(deps.length)
//      print("; ")
//      print(sbNr.toString())
//      print('\t')
//      println(sbDL.toString())
//    }
  }
}
