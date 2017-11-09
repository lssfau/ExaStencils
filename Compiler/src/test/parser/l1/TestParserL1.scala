package test.parser.l1

import exastencils.deprecated.ewald.L1_Parser

object TestParserL1 {
  val parser = L1_Parser

  val file =
    """Domain:
   //this is a comment
   Ω = (0,2) \times (0,2) // \times (0,4)

   Functions:
   g(U,a) = 5*fg(U,2);
   fg(U,b) = b^^3*U;

   Equation:
   \Delta(U) = 0
   // \partial{x_1^2,x_2^2} = 0

   Boundaries:
   U = sin(x_1) + sin(x_2)	for x_1=0 \lor x_1=2;
   U = sin(x_1) + sin(x_2) for x_2=0 \lor x_2=2;
   //∂_n U = 1	   		    for \partial\Omega;

   Options:
   N_0=3;
   \partial_0 = 0; // -1: backward, 0: central, 1: forward
   e_0 = 2;
   """

  def main(args : Array[String]) : Unit = {
    val x = parser.parseCode(file)
    System.out.println(x)
  }
}