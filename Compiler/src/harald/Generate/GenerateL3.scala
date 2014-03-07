package harald.Generate

import java.io._
import harald.dsl.DomainKnowledge
import harald.ast.TreeL2

class GenerateL3(treel2 : TreeL2) {

  def transformL2toL3(fname: String) {

    val writer = new PrintWriter(new File(fname))

    val fragname = DomainKnowledge.domain_L1.get._2 match {
      case "UnitSquare" => "Regular_Square"
      case "UnitCube" => "Regular_Cube"
    }

    writer.write("mgcomponents { \n")
    //writer.write("smoother_L3 = GaussSeidel \n")
    writer.write("interpolation_L3 = interpolatecorr  \n")
    writer.write("restriction_L3 = Restrict \n")
    writer.write("coarsesolver_L3 = GaussSeidel \n")
    writer.write("cycle_L3 = VCycle \n")
    writer.write("} \n")
    writer.write("\n")

    var nlev = 1
    var minsize: Int = Math.min(DomainKnowledge.xsize_L2.get, DomainKnowledge.ysize_L2.get)
    while (minsize > 2) {
      minsize /= 2
      nlev += 1
    }

    var order = 1
    for (o <- treel2.exaOperators)
      if (o.name.equals("RestrictionStencil"))
        order = o.order.toInt

    writer.write("mgparameter { \n")
    //writer.write(s"nlevels_L3 = ${nlev} \n")
    writer.write(s"restr_order_L3 = ${order} \n")
    writer.write(s"int_order_L3 = ${order} \n")
    //writer.write("ncoarse_L3 = 512 \n")
    //writer.write("nprae_L3 = 3 \n")
    //writer.write("npost_L3 = 3 \n")
    writer.write("iters_L3 = 10 \n")
    //writer.write("omega_L3 = 1.0 \n")
    writer.write("accuracy_L3 = 8 \n")
    writer.write("} \n")
    writer.write("\n")

    writer.close()
    println("DSL level 3 was generated in " + fname)
  }
}




