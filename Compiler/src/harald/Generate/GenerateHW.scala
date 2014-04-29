package harald.Generate

import java.io._
import harald.dsl.DomainKnowledge

case class OptionsHW(platform : String, cores:Int, nodes : Int)

object GenerateHW {
  
  def generateall(path:String, fname:String) {
    
    var num = 0;
    val platf = List("cpu","gpu")
    val core = List(1,2,4)
    val node = List(1,2)
    
    for (p <- platf)
    for (c <- core)
    for (n <- node) {
      num = num + 1
      transformhardwaretoHW(path+fname+p+c+n+"levHW.mg",OptionsHW(p,c,n))
    }
    
  }
  
  def transformhardwaretoHW(fname: String, opt : OptionsHW) {

    val writer = new PrintWriter(new File(fname))

    writer.write(s"Hardware ${opt.platform} {\n")
    writer.write(s" bandwidth_HW = 59\n")
    writer.write(s" peak_HW = 118\n")
    writer.write(s" cores_HW = ${opt.cores}\n")
    writer.write(s"}\n")
    writer.write("\n")

    writer.write(s"Node n {\n")
    writer.write(s"  sockets_HW = 1\n")  
    writer.write(s"}\n")
    writer.write("\n")

    writer.write(s"Cluster cluster {\n")
    writer.write(s"  nodes_HW = ${opt.nodes}\n")
    writer.write(s"  networkbandwidth_HW = 10\n")
    writer.write(s"}\n")

    writer.close()
    if (DomainKnowledge.debugmode)
      println("DSL level HW was generated in " + fname)
  }

}