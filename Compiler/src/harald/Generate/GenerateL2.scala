package harald.Generate

import java.io._
import harald.dsl.DomainKnowledge

object GenerateL2 {

  def transformL1toL2(fname: String) {

    val writer = new PrintWriter(new File(fname))

    val fragname = DomainKnowledge.domain_L1.get._2 match {
      case "UnitSquare" => "Regular_Square"
      case "UnitCube" => "Regular_Cube"
    }

    writer.write(s"Fragments f1 = ${fragname}\n")
    writer.write("\n")

    var size = 1
    //   println(Math.pow(10, DomainKnowledge.accuracy_L1.get))
    while (size < Math.pow(2, DomainKnowledge.accuracy_L1.get))
      size *= 2

    writer.write("Discrete_Domain d { \n")
    writer.write(s"xsize_L2 = ${size}  \n")
    writer.write(s"ysize_L2 = ${size}  \n")

    if (DomainKnowledge.rule_dim() == 3)
      writer.write(s"zsize_L2 = ${size}  \n")
    writer.write("} \n")

    writer.write("\n")
    var datatype = DomainKnowledge.getglobaldatatype._1
    val vectorentries = "1"
    val matrixentries = "1,1"
      
    for (i <- 0 to DomainKnowledge.function_L1.length - 1)
      writer.write(s"field<${datatype},${vectorentries}>@nodes ${DomainKnowledge.function_L1(i)._1}  \n")
    for (i <- 0 to DomainKnowledge.unknown_L1.length - 1)
      writer.write(s"field<${datatype},${vectorentries}>@nodes ${DomainKnowledge.unknown_L1(i)._1}  \n")
    writer.write(s"field<${datatype},${vectorentries}>@nodes Res  \n")
    for (i <- 0 to DomainKnowledge.operator_L1.length - 1)
      writer.write(s"stencil<${datatype},${matrixentries}, FD, 2>@nodes ${DomainKnowledge.operator_L1(i)._1}  \n")
    writer.write(s"stencil<${datatype},${matrixentries}, FD, 2>@nodes RestrictionStencil  \n")
    writer.write(s"stencil<${datatype},${matrixentries}, FD, 2>@nodes CorrectionStencil  \n")

    writer.close()
    println("DSL level 2 was generated in " + fname)
  }
}
