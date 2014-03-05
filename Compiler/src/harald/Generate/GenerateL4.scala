package harald.Generate

import java.io._
import scala.collection.mutable.ListBuffer
import harald.dsl.DomainKnowledge
import harald.ast.TreeL2
import exastencils.knowledge.Knowledge

class GenerateL4(treel2 : TreeL2) {

  def transformL3toL4(fname : String) {

    val writer = new PrintWriter(new File(fname))

    var location = DomainKnowledge.hardware_HW.get

    var veclength = 1
    for (f <- treel2.exaFields)
      if (f.name.equals(DomainKnowledge.unknown_L1(0)._1))
        veclength = f.veclength.toInt
    var matlength1 = 1
    var matlength2 = 1
    for (o <- treel2.exaOperators)
      if (o.name.equals(DomainKnowledge.operator_L1(0)._1)) {
        matlength1 = o.matlength1.toInt
        matlength2 = o.matlength2.toInt
      }

    if (DomainKnowledge.cycle_L3.get.equals("VCycle")) {

      writer.write(s"def cpu ${DomainKnowledge.cycle_L3.get} ( lev:Int  ) : Unit \n")
      writer.write("{ \n")
      writer.write(" if coarsestlevel { \n")
      writer.write("repeat up ncoarse \n")
      writer.write(s"	${DomainKnowledge.smoother_L3.get} ( lev) \n")
      writer.write("  next  \n")
      writer.write("} else \n")
      writer.write("{ \n")
      writer.write("  repeat up nprae \n")
      writer.write(s"	${DomainKnowledge.smoother_L3.get}( lev) \n")
      writer.write("  next  \n")
      writer.write("	Residual ( lev ) \n")
      if (veclength > 1) {
        for (i <- 0 to veclength - 1) {
          writer.write(s"	${DomainKnowledge.restriction_L3.get} ( lev \n")
          writer.write(s"	           ${DomainKnowledge.function_L1(0)._1}_${i}[ ( lev-1 ) ] \n")
          writer.write(s"	           Res_${i}[lev]) \n")
        }
      } else {
        writer.write(s"	${DomainKnowledge.restriction_L3.get} ( lev ) \n")
        //writer.write(s"	           ${DomainKnowledge.function_L1(0)._1}[ ( lev-1 ) ] \n")
        //writer.write(s"	           Res[lev]) \n")
      }
      val setname = location match { case "gpu" => "setcuda" case _ => "set" }
      writer.write(s"	${setname}( ( lev-1 ) \n")
      //writer.write(s"	    ${DomainKnowledge.unknown_L1(0)._1}[ ( lev - 1 ) ] \n")
      writer.write("	     0) \n")
      writer.write(s"	${DomainKnowledge.cycle_L3.get} ( lev-1 ) \n")
      writer.write(s"	${DomainKnowledge.interpolation_L3.get}( lev ) \n")
      //writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1}[lev] \n")
      //writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1}[ (lev-1) ] ) \n")
      writer.write("    repeat up npost \n")
      writer.write(s"		${DomainKnowledge.smoother_L3.get} ( lev ) \n")
      writer.write("  next  \n")
      writer.write("} \n")
      writer.write("} \n")
    }

    writer.write("\n")

    writer.write(s"def ${location} Residual ( lev:Int ) : Unit  \n")
    writer.write("{ \n")
    // COMM_HACK
    writer.write("  exchsolData ( lev \n 0 )  \n")
    writer.write("loop innerpoints level lev order lex block 1 1  \n")
    writer.write(s"  Res = ${DomainKnowledge.function_L1(0)._1} [ lev ] - (${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] )  \n")
    writer.write("next \n")
    writer.write("}  \n")

    writer.write("\n")
    /*def cpu L2Residual ( lev:Int ) : Double 
{ 
    Residual ( lev )
    Reduction loop innerpoints level lev order lex block 1 1 
        s += (Res [ lev ] ) * (Res [ lev ] ) 
    next  
}  
def cpu L2Residual ( lev:Int ) : Double 
{ 
    Residual ( lev )
    sqr ( lev 
         Res [lev ] )
    Reduction loop innerpoints level lev order lex block 1 1 
        s += (Res [ lev ] )  
    next  
}  
def cpu sqr ( lev:Int 
          arr:Array ) : Unit  
{ 
  loop allpoints level arr order lex block 1 1  
      arr = arr * arr    
  next  
}  
*/
    if (location.equals("gpu")) {
      writer.write(s"def cpu L2Residual ( lev:Int ) : Double \n")
      writer.write("{ \n")
      writer.write(" Residual ( lev ) \n")
      writer.write(" sqr ( lev  \n")
      writer.write("       Res [lev ] ) \n")
      writer.write("return fasterReduce ( Res ) \n")
      writer.write("}  \n")

      //  std::cout << "Res" << fasterReduce (Res[lev].begin(), solution[lev].x1_*solution[lev].x2_, f[lev].begin()) << std::endl;

      writer.write("\n")

      writer.write(s"def ${location} sqr ( lev:Int \n")
      writer.write("          arr:Array) : Unit  \n")
      writer.write("{ \n")
      writer.write("  loop allpoints level arr order lex block 1 1  \n")
      writer.write("      arr = arr * arr    \n")
      writer.write("next  \n")
      writer.write("}  \n")

      writer.write("\n")

    } else {

      writer.write(s"def ${location} L2Residual ( lev:Int ) : Double \n")
      writer.write("{ \n")
      // COMM_HACK
      writer.write("  exchsolData ( lev \n 0 )  \n")
      writer.write("    Reduction loop innerpoints level lev order lex block 1 1 \n")
      writer.write(s"        s += (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) * (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) \n")
      writer.write("    next  \n")
      writer.write("}  \n")
    }

    if (DomainKnowledge.smoother_L3.get.equals("GaussSeidel")) {
      writer.write(s"def ${location} ${DomainKnowledge.smoother_L3.get} ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      // COMM_HACK
      writer.write("  exchsolData ( lev \n 0 )  \n")
      writer.write("    loop innerpoints level lev order lex block 1 1 \n")
      writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1} = ${DomainKnowledge.unknown_L1(0)._1} [ lev ] + ( ( ( inverse( diag(${DomainKnowledge.operator_L1(0)._1} [ lev ] ) ) ) * omega ) * ( ${DomainKnowledge.function_L1(0)._1} [ lev ] - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] ) ) \n")
      writer.write("    next  \n")
      writer.write("}  \n")
    }

    writer.write("\n")

    // COMM_HACK
    writer.write(s"def ${location} ${DomainKnowledge.restriction_L3.get} ( lev:Int ) : Unit \n")
    //writer.write("               coarse:Container \n")
    //writer.write("               fine:Container) : Unit  \n")
    writer.write("{ \n")
    // COMM_HACK
    writer.write("  exchresData ( lev \n 0 )  \n")
    writer.write("    loop innerpoints level solutionMinusOne order lex block 1 1  \n")
    // COMM_HACK
    //writer.write("      coarse =  RestrictionStencil * fine | ToCoarse  \n")
    writer.write(s"      fMinusOne =  RestrictionStencil * fine | ToCoarse  \n")	// fMinusOne represents f [ lev - 1 ] which is not parsable :/
    writer.write("    next  \n")
    writer.write("}  \n")

    writer.write("\n")

    // COMM_HACK
    writer.write(s"def ${location} ${DomainKnowledge.interpolation_L3.get}( lev:Int ) : Unit \n")
    //writer.write("                     uf:Container  \n")
    //writer.write("                     uc:Container ) : Unit \n")
    writer.write("{ \n")
    // COMM_HACK
    writer.write("  exchsolData ( (lev-1) \n 0 )  \n")
    writer.write("    loop innerpoints level lev order lex block 1 1  \n")
    writer.write(s"    ${DomainKnowledge.unknown_L1(0)._1} += RestrictionStencil * ${DomainKnowledge.unknown_L1(0)._1} [ (lev - 1) ] | ToFine  \n")
    writer.write("    next  \n")
    writer.write("}  \n")

    writer.write("\n")

    var setfunclistloc : ListBuffer[String] = ListBuffer(s"${location}")
    if (location.equals("gpu"))
      setfunclistloc += "cpu"

    for (n <- setfunclistloc) {
      val setname = n match { case "gpu" => "setcuda" case _ => "set" }
      writer.write(s"def ${n} ${setname} ( lev:Int \n")
      // COMM_HACK
      //writer.write("          arr:Container  \n")
      writer.write("          value:Int) : Unit  \n")
      writer.write("{ \n")
      writer.write("  loop allpoints level lev order lex block 1 1  \n")
      writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1} = value    \n")
      writer.write("next  \n")
      writer.write("}  \n")

      writer.write("\n")
    }

    // COMM_HACK
    writer.write("def cpu setrandom(arr:Container \n")
    writer.write("               v:Int) : Unit \n")
    writer.write("{ \n")
    writer.write("  loop innerpoints level arr order lex block 1 1  \n")
    writer.write("      arr = random(v)    \n")
    writer.write("  next  \n")
    writer.write("} \n")

    writer.write("\n")
    /*
    writer.write(s"def cpu print ( lev:Int \n")
    writer.write("          arr:Array) : Unit  \n")
    writer.write("{ \n")
    writer.write("  loop allpoints level arr order lex block 1 1  \n")
    writer.write("     print ( arr )    \n")
    writer.write("next  \n")
    writer.write("}  \n")
*/

    // Generate Application!

    // COMM_HACK
    writer.write(s"def cpu Application ( ) : Unit \n")
    writer.write("{  \n")
    writer.write(s" decl res0 : Double = sqrt ( L2Residual ( ${Knowledge.maxLevel} ) ) \n")
    writer.write(" decl res : Double = res0 \n")
    writer.write(" decl resold : Double = 0 \n")
    writer.write(" print ( 'startingres' res0 ) \n")
    writer.write(" repeat up 10 \n")
    writer.write(" resold = res \n")
    writer.write(s"VCycle ( ${Knowledge.maxLevel} ) \n")
    writer.write(s"res = sqrt ( L2Residual ( ${Knowledge.maxLevel} ) ) \n")
    writer.write("print ( 'Residual:' res 'residual reduction:' (res0/res) 'convergence factor:' (res/resold) ) \n")
    writer.write("  next  \n")
    writer.write("}  \n")

    writer.close()
    println("DSL level 4 was generated in " + fname)
  }

}