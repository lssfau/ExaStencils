package harald.Generate

import java.io._
import harald.dsl.DomainKnowledge

case class OptionsL1(domain : String, operator : String, bc : String, accuracy : Int, generate : Int)

object GenerateL1 {

  def generateall(path : String, fname : String) = {

    var num = 0;
    val dom = List("UnitSquare", "UnitCube")
    val op = List("Laplacian", "ComplexDiffusion")
    val bc = List("zero", "dn")
    val acc = List(4, 5, 6, 7, 8, 9)
    val gen = List(0, 1)

    for (d <- dom)
      for (o <- op)
        for (b <- bc)
          for (a <- acc)
            for (g <- gen) {
              num = num + 1
              transformproblemtoL1(path + fname + d + o + b + a + g + "lev1.mg", OptionsL1(d, o, b, a, g))
            }

  }

  def transformproblemtoL1(fname : String, opt : OptionsL1) = {

    val writer = new PrintWriter(new File(fname))

    val domstr = "dom"
    //    writer.write(s"Domain ${domstr} = ${opt.domain}\n")
    writer.write(s"Domain ${domstr} = [0,1] x [0,2]\n")
    writer.write("\n")

    if (opt.operator.equals("Laplacian")) {
      writer.write(s"FT f : ${domstr}^1 -> R^1\n")
      writer.write(s"FT u : ${domstr}^1 -> R^1\n")
      writer.write(s"OT Laplacian : ( ${domstr}^1 -> R^1 ) -> ( ${domstr}^1 -> R^1 )\n")
      writer.write(s"OP Laplacian <0> = 'dx2' + 'dy2'\n")
      writer.write("\n")
      writer.write(s"EQ pde : Laplacian [ u ] = f in ${domstr}\n")
      writer.write(s"EQ bc : u = 0 in d${domstr}\n")
    } else if (opt.operator.equals("ComplexDiffusion")) {

      writer.write(s"FT f : C^1 -> C^1\n")
      writer.write(s"FT u : C^1 -> C^1\n")
      writer.write(s"FT g : C^1 -> C^1\n")
      writer.write("OT nabla : ( 1 -> R^1 ) -> ( 2 -> R^2 )\n");
      writer.write("OT ComplexDiffusion : ( 1 -> R^1 ) -> ( 2 -> R^2 )\n");
      writer.write("CO sigma : R^1\n");
      writer.write("CO kdiff : R^1\n");
      writer.write("OP nabla <0> = 'dx'\n");
      writer.write("OP nabla <1> = 'dy'\n");
      writer.write(s"OP ComplexDiffusion <0> = g * nabla <0> \n")
      writer.write(s"OP ComplexDiffusion <1> = g * nabla <1> \n")
      writer.write("FU g<0> = ( cos { sigma } ) * (( 1.0 ) / ( (1.0) + ( ( u<1> * u<1> ) * ( ( 1.0 ) / ( (kdiff*sigma) * (kdiff*sigma) ) ) ) ) )\n");
      writer.write("FU g<1> = ( sin { sigma } ) * (( 1.0 ) / ( (1.0) + ( ( u<1> * u<1> ) * ( ( 1.0 ) / ( (kdiff*sigma) * (kdiff*sigma) ) ) ) ) )\n");
      writer.write("\n");
      //writer.write(s"EQ pde : integral d${domstr} ( ( g * nabla [ u ] ) * 'n' ) = integral ${domstr} f in ${domstr}\n")
      writer.write(s"EQ pde : integral d${domstr} ComplexDiffusion [ u ]  = integral ${domstr} f in ${domstr}\n")
      writer.write(s"EQ bc : u = 0 in d${domstr}\n")

    } else if (opt.operator.equals("OpticalFlow")) {
      writer.write("FT I : Z^1 -> R^1\n");
      writer.write("FT u : R^2 -> R^2\n");
      writer.write("FT f : R^2 -> R^2\n");
      writer.write("FT gradI : R^2 -> R^2\n");
      writer.write("FT gradIt : R^1 -> R^1\n");
      writer.write("OT OptFlow : ( 2 -> R^2 ) -> ( 2 -> R^2 )\n");
      writer.write("OT Laplace : ( 2 -> R^2 ) -> ( 2 -> R^2 )\n");
      writer.write("OT nabla : ( 1 -> R^1 ) -> ( 2 -> R^2 )\n");
      writer.write("OT nablat : ( 1 -> R^1 ) -> ( 1 -> R^1 )\n");
      writer.write("CO alpha : R^1\n");
      writer.write("\n");
      writer.write("OP Laplace <0> = 'dx2' + 'dy2'\n");
      writer.write("OP Laplace <1> = '0'\n");
      writer.write("OP Laplace <2> = '0'\n");
      writer.write("OP Laplace <3> = 'dx2' + 'dy2' \n");
      writer.write("OP nabla <0> = 'dx'\n");
      writer.write("OP nabla <1> = 'dy'\n");
      writer.write("OP nablat <0> = 'dt'\n");
      writer.write("OP OptFlow <0> = ( alpha * (- Laplace <0> ) ) + ( gradI<0> * gradI<0> )\n");
      writer.write("OP OptFlow <1> = ( gradI<0> * gradI<1> ) \n");
      writer.write("OP OptFlow <2> = ( gradI<0> * gradI<1> ) \n");
      writer.write("OP OptFlow <3> = ( alpha * (- Laplace <3> ) ) + ( gradI<1> * gradI<1> )\n");
      writer.write("FU gradI<0> = nabla <0> [ I ]\n");
      writer.write("FU gradI<1> = nabla <1> [ I ]\n");
      writer.write("FU gradIt<0> = nablat <0> [ I ]\n");
      writer.write("FU f<0> = - ( gradI<0> * gradIt<0> )\n");
      writer.write("FU f<1> = - ( gradI<1> * gradIt<0> )\n");
      writer.write(s"EQ pde : OptFlow [ u ] = f in ${domstr}\n");
      writer.write(s"EQ def1 : gradI = nabla [ I ] in ${domstr}\n");
      writer.write(s"EQ def2 : gradIt = nablat [ I ]  in ${domstr}\n");
      writer.write(s"EQ bc : integral domega nabla [ u ] * 'n' = 0 in d${domstr}\n");

    }
    /*    writer.write(s"Function f = 0\n")
    writer.write(s"Unknown solution = 1\n")
    
    writer.write(s"Operator Lapl = ${opt.operator}\n")
    writer.write("\n")

    writer.write("PDE pde { Lapl(solution) = f }\n") 
   writer.write(s"PDEBC bc { solution = ${opt.bc} }\n")
    writer.write("\n")

   writer.write("System = 1\n")
*/
    writer.write(s"Accuracy = ${opt.accuracy}\n")
    writer.write(s"Generate = ${opt.generate}\n")

    writer.close()
    // if (DomainKnowledge.debugmode)
    println("DSL level 1 was generated in " + fname)
  }

}