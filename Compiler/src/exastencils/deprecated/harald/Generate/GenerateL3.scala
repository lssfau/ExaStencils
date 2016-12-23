package exastencils.deprecated.harald.Generate

import java.io._
import exastencils.deprecated.harald.dsl.DomainKnowledge
import exastencils.deprecated.harald.ast.TreeL2

case class OptionsL3(smoother : String, omega : Double, nprae : Int, npost : Int, cycletype : String, coarsesolver : String, iterscoarse : Int, iters : Int, accuracy : Int)

class GenerateL3(treel2 : TreeL2) {

  def generateall(path : String, fname : String) = {

    var num = 0;
    val sm = List("GaussSeidel", "Jacobi")
    val om = List(0.8, 1.0, 1.2)
    val nprae = List(1, 2, 3)
    val npost = List(1, 2, 3)
    val cy = List("VCycle", "FASVCycle")
    //    val coarse = List()
    val iters = List(1, 5, 10)
    val iterscoarse = List(1, 5, 10)
    val acc = List(8)

    for (s <- sm)
      for (o <- om)
        for (nae <- nprae)
          for (nost <- npost)
            for (c <- cy)
              for (i <- iters)
                for (ic <- iterscoarse)
                  for (a <- acc) {
                    transformL2toL3opt(path + fname + s + o + nae + nost + c + i + a + ".mg", OptionsL3(s, o, nae, nost, c, s, ic, i, a))
                  }

  }

  def getoptionsdefault() : OptionsL3 = OptionsL3("GaussSeidel", 1.0, 2, 2, "VCycle", "GaussSeidel", 3, 5, 8)

  def transformL2toL3(fname : String) = {
    val opt = getoptionsdefault
    transformL2toL3opt(fname, opt)
  }

  def transformL2toL3aopt(fname : String, opt : OptionsL3) = {

    val writer = new PrintWriter(new File(fname))

    var smoother = opt.smoother
    var coarsesolver = opt.smoother
    var smootheromega = opt.omega
    var nprae = opt.nprae
    var npost = opt.npost
    var cycletype = opt.cycletype
    var cyclerep = 1 // for Vcycle

    var operator = DomainKnowledge.operator_L1(0)._1
    var unknown = DomainKnowledge.unknown_L1(0)._1
    var function = DomainKnowledge.function_L1(0)._1

    /*  
    writer.write("mgparameter { \n")
    writer.write(s"restr_order_L3 = ${orderrestr} \n")
    writer.write(s"int_order_L3 = ${orderint} \n")
    writer.write(s"iters_L3 = ${opt.iters} \n")
    writer.write(s"accuracy_L3 = ${opt.accuracy} \n")
*/
    writer.write(s"VEC res SIZE ${unknown} \n");
    writer.write(s"MATRIX N SIZE ${operator} \n");
    writer.write("MATRIX M SIZE Laplacian \n");
    writer.write(s"RESTRMATRIX R of ${unknown} = 2 \n");
    writer.write("SET s = [0,0][1,1] \n");
    writer.write("SET sred = [0,0][2,2] [1,1][2,2] \n");
    writer.write("SET sblack = [0,1][2,2] [1,0][2,2] \n");
    writer.write("\n")
    writer.write(s"ITERATION ismootherred : ${unknown} [l] = ( ( I - ( ( (${smootheromega}) * ( inverse(diag(${operator} [l])) ) ) *${operator} [l] ) ) *${unknown} [l] ) + ( ( (${smootheromega}) * ( inverse(diag(${operator} [l])) ) ) *${function} [l] ) order sred \n");
    writer.write(s"ITERATION ismootherblack : ${unknown} [l] = ( ( I - ( ( (${smootheromega}) * ( inverse(diag(${operator} [l])) ) ) *${operator} [l] ) ) *${unknown} [l] ) + ( ( (${smootheromega}) * ( inverse(diag(${operator} [l])) ) ) *${function} [l] ) order sblack \n");
    writer.write(s"ITERATION ${smoother} : (ismootherred [l]) ~ (ismootherblack [l] ) \n");
    writer.write(s"ITERATION iprolong : ${unknown} [l] = ${unknown} [l] + ( ( transp (R) ) * ${unknown} [lc]) order s \n");
    writer.write(s"ITERATION iresidual : res [l] = ${function} [l] - (${operator} [l] * ${unknown} [l]) order s \n");
    writer.write(s"ITERATION irestrict : ${function} [lc] = (R * res [l]) order s \n");
    //writer.write(s"ITERATION iclev :  ( ( ((${smoother} [l])^${nprae} ) ~ (iresidual [l]) ) ~ ( (irestrict [l]) ~ ( (icycle [lc])^${cyclerep} ) ) ) ~ ( (iprolong [l]) ~ ( (${smoother} [l])^${npost} ) )\n");
    writer.write(s"ITERATION icycle : if (l == ${DomainKnowledge.nlevels_L2.getOrElse(1) - 1}) then (${coarsesolver} [l])^${opt.iterscoarse} \n");
    //writer.write(s"                  if (l != ${DomainKnowledge.nlevels_L2.getOrElse(1)-1}) then ( iclev )\n");
    writer.write(s"                  if (l != ${DomainKnowledge.nlevels_L2.getOrElse(1) - 1}) then ( ( ((${smoother} [l])^${nprae} ) ~ (iresidual [l]) ) ~ ( (irestrict [l]) ~ ( (icycle [lc])^${cyclerep} ) ) ) ~ ( (iprolong [l]) ~ ( (${smoother} [l])^${npost} ) )\n");
    writer.write("\n")

    writer.close()
    if (DomainKnowledge.debugmode)
      println("DSL level 3a was generated in " + fname)
  }

  def transformL2toL3opt(fname : String, opt : OptionsL3) = {

    val writer = new PrintWriter(new File(fname))
    /*
    val fragname = DomainKnowledge.domain_L1.get._2 match {
      case "UnitSquare" => "Regular_Square"
      case "UnitCube" => "Regular_Cube"
    }
  */
    var smoother = opt.smoother
    var smootheromega = opt.omega
    var nprae = opt.nprae
    var npost = opt.npost
    var cycletype = opt.cycletype
    /*    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion") || DomainKnowledge.use_Openmp) {
      smoother = "Jacobi"
      smootheromega = 0.8  
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) 
        cycletype = "FASVCycle"
    }
  */
    writer.write("mgcomponents { \n")
    writer.write(s"smoother_L3 = ${smoother} \n")
    //    writer.write("interpolation_L3 = interpolatecorr  \n")
    //    writer.write("restriction_L3 = Restrict \n")
    //    writer.write(s"coarsesolver_L3 = ${smoother} \n")
    writer.write(s"cycle_L3 = ${cycletype} \n")
    writer.write("} \n")
    writer.write("\n")

    var orderrestr = 2
    var orderint = 2
    for (o <- treel2.exaOperators)
      if (o.name.equals("RestrictionStencil")) {
        orderrestr = o.order.toInt
        if (o.location.equals("cells"))
          orderint = 1
      }

    writer.write("mgparameter { \n")
    //    writer.write(s"restr_order_L3 = ${orderrestr} \n")
    //    writer.write(s"int_order_L3 = ${orderint} \n")
    //    writer.write(s"ncoarse_L3 = ${opt.iterscoarse} \n")
    //    writer.write(s"nprae_L3 = ${nprae} \n")
    //    writer.write(s"npost_L3 = ${npost} \n")
    writer.write(s"iters_L3 = ${opt.iters} \n")
    writer.write(s"accuracy_L3 = ${opt.accuracy} \n")
    writer.write("} \n")
    writer.write("\n")

    /*    writer.write("mgparameterfloat { \n")
    writer.write(s"omega_L3 = ${smootheromega} \n")
    writer.write("} \n")
    writer.write("\n")
  */
    writer.close()
    if (DomainKnowledge.debugmode)
      println("DSL level 3 was generated in " + fname)
  }
}




