package harald.dsl

import scala.collection.mutable.ListBuffer
import harald.ast.TreeL2

class PerformanceModel(treel2 : TreeL2) {
   
  def estimatePerformance(func: String): (Double, Double) = {

    var gscostsIO : Long = 0
    var gscostsFLOP : Long = 0

    for (c <- treel2.Functions) {
      if (c._2.name.equals("VCycle")) {
        //println(c._2.name)
        var paras: ListBuffer[ParameterInfo] = ListBuffer(new ParameterInfo("lev", "int", 0))
        c._2.calls(0)
      } else if (c._2.name.equals(func)) {
        for (m <- c._2.costs("Store"))
          gscostsIO += m._1
        for (m <- c._2.costs("Load"))
          gscostsIO += m._1
          
        for (m <- c._2.costs("-"))
          gscostsFLOP += m._1
        for (m <- c._2.costs("+"))
          gscostsFLOP += m._1
        for (m <- c._2.costs("*"))
          gscostsFLOP += m._1
        for (m <- c._2.costs("/"))
          gscostsFLOP += m._1
        println(s"Costs ${func} Load = " + c._2.costs("Load"))
        println(s"Costs ${func} Store = " + c._2.costs("Store"))
        println(s"Costs ${func} * = " + c._2.costs("*"))
        println(s"Costs ${func} / = " + c._2.costs("/"))
        println(s"Costs ${func} + = " + c._2.costs("+"))
        println(s"Costs ${func} - = " + c._2.costs("-"))
      }
    }

    var gscosts : Long = 0
    println(treel2.callgraph.get(func).get)
    for (m <- treel2.callgraph.get(func).get) {
      gscosts += m._2 * DomainKnowledge.arraysizes(m._1)(0)
      // println(m._2.toString + " "  + m._1.toString  + " " + DomainKnowledge.arraysizes(m._1)(0))
    }

    val bytes : Long = DomainKnowledge.getglobaldatatype._2
    val sumcostsIO : Long = gscosts * bytes * gscostsIO
    val sumcostsFLOP : Long = gscosts * gscostsFLOP

    println(s"${func} Costs: Points: ${gscosts} IO: ${gscostsIO} bytes: ${bytes} overall: ${sumcostsIO} Bytes ")
    println(s"${func} Costs: Points: ${gscosts} FLOP: ${gscostsFLOP} overall: ${sumcostsFLOP} FLOP ")

    val timeIO: Double = sumcostsIO / (1000000.0 * DomainKnowledge.bandwidth_HW.get.toDouble)
    val timeFLOP: Double = sumcostsFLOP / (1000000.0 * DomainKnowledge.peak_HW.get.toDouble)

    //		println(TransformL4.callgraph)
    //		println( DomainKnowledge.arraysizes )
    return timeIO -> timeFLOP
  }
}