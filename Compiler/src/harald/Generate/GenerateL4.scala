package harald.Generate

import java.io._
import scala.collection.mutable.ListBuffer
import harald.dsl.DomainKnowledge
import harald.ast.TreeL2

class GenerateL4(treel2 : TreeL2) {

  def transformL3toL4_old(fname : String) {

    val writer = new PrintWriter(new File(fname))

    var location = DomainKnowledge.hardware_HW.get

    var hoststr = ""
    if (location.equals("gpu"))
      hoststr = "_host"

    var compstr = ""
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

    var restrstr = "Restriction"
    var intstr = "interpolation"

    var restrfactor = "0.25"
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      restrfactor = "1.0"

    if (DomainKnowledge.cycle_L3.get.equals("VCycle") || DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGVCycle")) {

      writer.write(s"def cpu ${DomainKnowledge.cycle_L3.get}_0 ( lev:Int  ) : Unit \n")
      writer.write("{ \n")
      writer.write(" if coarsestlevel { \n")
      writer.write("repeat up i ncoarse \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        if (location.equals("gpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
      writer.write(s"	${DomainKnowledge.smoother_L3.get} ( lev) \n")
      writer.write("  next  \n")
      writer.write("} else \n")
      writer.write("{ \n")
      writer.write("  repeat up i nprae \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        if (location.equals("gpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
      writer.write(s"	${DomainKnowledge.smoother_L3.get}( lev) \n")
      writer.write("  next  \n")

      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        if (location.equals("gpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")

      writer.write("	Residual ( lev ) \n")

      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>"
        writer.write(s"	${restrstr} ( ( lev+1 ) \n")
        writer.write(s"	           ${DomainKnowledge.function_L1(0)._1} ${compstr} [ ( lev+1 ) ] \n")
        writer.write(s"	           Res ${compstr} [lev] \n")
        writer.write(s"	           ( ${restrfactor} ) ) \n")
        if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
          writer.write(s"	${restrstr} ( ( lev+1 ) \n")
          writer.write(s"	           ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [ ( lev+1 ) ] \n")
          writer.write(s"	           ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [lev] \n")
          writer.write(s"	           ( ${restrfactor} ) ) \n")
        }
      }
      if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle"))
        writer.write(s"	RHS_FAS ( lev+1 ) \n")
      else {
        compstr = ""
        val setname = location match { case "gpu" => "setcuda" case _ => "set" }
        for (i <- 0 to veclength - 1) {
          if (veclength > 1)
            compstr = s"<${i}>"

          writer.write(s"	${setname}( ( lev+1 ) \n")
          writer.write(s"	    ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [ ( lev + 1 ) ] \n")
          writer.write("	     0) \n")
        }
      }

      writer.write(s"	${DomainKnowledge.cycle_L3.get} ( lev+1 ) \n")

      compstr = ""
      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>"

        if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
          writer.write(s"	${restrstr} ( ( lev+1 ) \n")
          writer.write(s"	           Res ${compstr} [ ( lev+1 ) ]  \n")
          writer.write(s"	           ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [lev] \n")
          writer.write(s"	           ( ${restrfactor} ) ) \n")
        }
      }

      if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle"))
        writer.write(s"	corr_FAS ( lev+1 ) \n")

      compstr = ""
      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>"

        writer.write(s"	${intstr}( lev \n")
        writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [lev] \n")
        writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [ (lev+1) ] ) \n")

      }

      writer.write("    repeat up i npost \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        if (location.equals("gpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
      writer.write(s"		${DomainKnowledge.smoother_L3.get} ( lev ) \n")
      writer.write("  next  \n")
      writer.write("} \n")
      writer.write("} \n")
    }

    writer.write("\n")

    writer.write(s"def ${location} Residual ( lev:Int ) : Unit  \n")
    writer.write("{ \n")

    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
      else if (location.equals("cpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}[lev] ) \n")

    writer.write("loop innerpoints level lev order lex block 1 1  \n")
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
    writer.write(s"  Res = ${DomainKnowledge.function_L1(0)._1} [ lev ] - (${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] )  \n")
    writer.write("next \n")
    writer.write("}  \n")

    writer.write("\n")

    if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
      writer.write("\n")

      writer.write(s"def ${location} RHS_FAS ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write("loop innerpoints level lev order lex block 1 1  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      writer.write(s"  ${DomainKnowledge.function_L1(0)._1} = ${DomainKnowledge.function_L1(0)._1} [ lev ] + (${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] )  \n")
      writer.write(s"  Res = ${DomainKnowledge.unknown_L1(0)._1} [ lev ]  \n")
      writer.write("next \n")
      writer.write("}  \n")

      writer.write("\n")

      writer.write(s"def ${location} corr_FAS ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write("loop innerpoints level lev order lex block 1 1  \n")
      writer.write(s"  ${DomainKnowledge.unknown_L1(0)._1}  = ${DomainKnowledge.unknown_L1(0)._1} [ lev ] - Res [ lev ] \n")
      writer.write("next \n")
      writer.write("}  \n")

      writer.write("\n")

    }

    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) {
      writer.write(s"def ${location} setcoeff ( lev:Int \n")
      writer.write("          arr:Array) : Unit  \n")
      writer.write("{ \n")
      writer.write(s" decl sigma : ${DomainKnowledge.abstracttype} = (0.1) \n")
      writer.write(s" decl solim : ${DomainKnowledge.abstracttype} = 1 \n")
      writer.write(" decl kdiff : Int = 5 \n")
      writer.write(s" decl kdiffs : ${DomainKnowledge.abstracttype} = (kdiff*sigma) \n")
      writer.write(s" decl sigmakdiffinv : ${DomainKnowledge.abstracttype} = ( ( 1.0 ) / ( kdiffs * kdiffs) ) \n")
      writer.write("  loop innerpoints level arr order lex block 1 1  \n")
      writer.write(s" solim = arr | Im \n")
      writer.write(s" decl sols : ${DomainKnowledge.abstracttype} = ( ( solim * solim ) * sigmakdiffinv ) \n")
      writer.write(s" decl denominv : ${DomainKnowledge.abstracttype} = (( 1.0 ) / ( (1.0) + sols)) \n")
      writer.write("      coeff =  ( ( cos(sigma))  * denominv ) | Re  \n") // real
      writer.write("      coeff =  ( ( sin ( sigma )) * denominv ) | Im    \n") // imag
      writer.write("next  \n")
      writer.write("}  \n")

      writer.write("\n")
    }

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
      writer.write(s"def cpu L2Residual ( lev:Int ) : ${DomainKnowledge.abstracttype} \n")
      writer.write("{ \n")
      writer.write(" Residual ( lev ) \n")
      writer.write(" sqr ( lev  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) {
        writer.write("       Res <0> [lev ] ) \n")
      } else
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

      writer.write(s"def ${location} L2Residual ( lev:Int ) : ${DomainKnowledge.abstracttype} \n")
      writer.write("{ \n")
      writer.write("    Reduction loop innerpoints level lev order lex block 1 1 \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      if (veclength > 1)
        writer.write(s"        s += (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) * (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) | Vec2 \n")
      else
        writer.write(s"        s += (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) * (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) \n")
      writer.write("    next  \n")
      writer.write("}  \n")
    }

    if (DomainKnowledge.smoother_L3.get.equals("GaussSeidel")) {
      writer.write(s"def ${location} ${DomainKnowledge.smoother_L3.get} ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1} lev )  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        if (location.equals("gpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
        else if (location.equals("cpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}[lev] ) \n")

      writer.write("    loop innerpoints level lev order rb block 1 1 \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1} = ${DomainKnowledge.unknown_L1(0)._1} [ lev ] + ( ( ( inverse( diag(${DomainKnowledge.operator_L1(0)._1} [ lev ] ) ) ) * omega ) * ( ${DomainKnowledge.function_L1(0)._1} [ lev ] - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] ) ) \n")
      writer.write("    next  \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1} lev )  \n")
      writer.write("}  \n")
    } else if (DomainKnowledge.smoother_L3.get.equals("Jacobi")) {
      writer.write(s"def cpu ${DomainKnowledge.smoother_L3.get} ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write(s" ${DomainKnowledge.smoother_L3.get}_1 ( lev ) \n")
      writer.write(s" ${DomainKnowledge.smoother_L3.get}_2 ( lev ) \n")
      writer.write("}  \n")

      writer.write(s"def ${location} ${DomainKnowledge.smoother_L3.get}_1 ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1} lev )  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        if (location.equals("gpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
        else if (location.equals("cpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}[lev] ) \n")

      writer.write("    loop innerpoints level lev order lex block 1 1 \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1}_old = ${DomainKnowledge.unknown_L1(0)._1} [ lev ] + ( ( ( inverse( diag(${DomainKnowledge.operator_L1(0)._1} [ lev ] ) ) ) * omega ) * ( ${DomainKnowledge.function_L1(0)._1} [ lev ] - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] ) ) \n")
      writer.write("    next  \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1}_old lev )  \n")
      writer.write("}  \n")

      writer.write(s"def ${location} ${DomainKnowledge.smoother_L3.get}_2 ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1}_old lev )  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        if (location.equals("gpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}_old <1> [lev] ) \n")
        else if (location.equals("cpu"))
          writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}_old[lev] ) \n")

      writer.write("    loop innerpoints level lev order lex block 1 1 \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1} = ${DomainKnowledge.unknown_L1(0)._1}_old [ lev ] + ( ( ( inverse( diag(${DomainKnowledge.operator_L1(0)._1} [ lev ] ) ) ) * omega ) * ( ${DomainKnowledge.function_L1(0)._1} [ lev ] - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1}_old [ lev ] ) ) \n")
      writer.write("    next  \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1} lev )  \n")
      writer.write("}  \n")

    }

    writer.write("\n")

    writer.write(s"def ${location} ${restrstr} ( lev:Int \n")
    writer.write("               coarse:Array \n")
    writer.write("               fine:Array  \n")
    writer.write(s"               factor:${DomainKnowledge.abstracttype}) : Unit  \n")
    writer.write("{ \n")
    writer.write("    loop innerpoints level coarse order lex block 1 1  \n")
    writer.write("      coarse =  ( factor * ( RestrictionStencil * fine ) ) | ToCoarse  \n")
    writer.write("    next  \n")
    writer.write("}  \n")

    writer.write("\n")

    writer.write(s"def ${location} ${intstr}( lev:Int \n")
    writer.write("                     uf:Array  \n")
    writer.write("                     uc:Array ) : Unit \n")
    writer.write("{ \n")
    writer.write("    loop innerpoints level uf order lex block 1 1  \n")
    writer.write("    uf += RestrictionStencil * uc | ToFine  \n")
    writer.write("    next  \n")
    writer.write("}  \n")

    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
      writer.write(s"def ${location} interpolate ( lev:Int \n")
      writer.write("                     uf:Array  \n")
      writer.write("                     uc:Array ) : Unit \n")
      writer.write("{ \n")
      writer.write("    loop innerpoints level uf order lex block 1 1  \n")
      writer.write("    uf = RestrictionStencil * uc | ToFine  \n")
      writer.write("    next  \n")
      writer.write("}  \n")
    }
    writer.write("\n")

    var setfunclistloc : ListBuffer[String] = ListBuffer(s"${location}")
    if (location.equals("gpu"))
      setfunclistloc += "cpu"

    for (n <- setfunclistloc) {
      val setname = n match { case "gpu" => "setcuda" case _ => "set" }
      writer.write(s"def ${n} ${setname} ( lev:Int \n")
      writer.write("          arr:Array  \n")
      writer.write("          value:Int) : Unit  \n")
      writer.write("{ \n")
      writer.write("  loop allpoints level arr order lex block 1 1  \n")
      writer.write("      arr = value    \n")
      writer.write("next  \n")
      writer.write("}  \n")

      writer.write("\n")
    }

    writer.write("def cpu setrandom(arr:Array  \n")
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

    // println(DomainKnowledge.operator_L1(0)._2)

    // check solution!
    //       if (DomainKnowledge.operator_L1(0)._2(0).equals("Laplacian")) {
    if (DomainKnowledge.rule_dim == 2) {
      writer.write(s"    def cpu getsolution ( x:${DomainKnowledge.abstracttype}\n")
      writer.write(s"                  y:${DomainKnowledge.abstracttype} ) :  ${DomainKnowledge.globaldatatype_L2} \n")
      writer.write("{ \n")
      if (DomainKnowledge.pdebc_L1.get._2.equals("zero"))
        writer.write("  return ( sin ( MATHPI * x ) ) * ( sin ( MATHPI * y ) )\n")
      else if (DomainKnowledge.pdebc_L1.get._2.equals("dn"))
        writer.write("  return ( cos ( MATHPI * x ) ) * ( cos ( MATHPI * y ) )\n")
      writer.write("}  \n")

      writer.write(s"def cpu getRHS ( x:${DomainKnowledge.abstracttype} \n")
      writer.write(s"                  y:${DomainKnowledge.abstracttype} ) :  ${DomainKnowledge.globaldatatype_L2}  \n")
      writer.write("{ \n")
      if (DomainKnowledge.pdebc_L1.get._2.equals("zero"))
        writer.write(" return ( ( 2 * ( MATHPI * MATHPI ) ) * ( ( sin ( MATHPI * x ) ) * ( sin ( MATHPI * y ) ) ) ) \n")
      else if (DomainKnowledge.pdebc_L1.get._2.equals("dn"))
        writer.write(" return ( ( 2 * ( MATHPI * MATHPI ) ) * ( ( cos ( MATHPI * x ) ) * ( cos ( MATHPI * y ) ) ) ) \n")
      writer.write("}  \n")
    } else if (DomainKnowledge.rule_dim == 3) {
      writer.write(s"    def cpu getsolution ( x:${DomainKnowledge.abstracttype}\n")
      writer.write(s"                  y:${DomainKnowledge.abstracttype} \n")
      writer.write(s"                  z:${DomainKnowledge.abstracttype} ) :  ${DomainKnowledge.globaldatatype_L2} \n")
      writer.write("{ \n")
      writer.write("  return ( sin ( MATHPI * x ) ) * ( ( sin ( MATHPI * y ) ) * ( sin ( MATHPI * z ) ) )\n")
      writer.write("}  \n")

      writer.write(s"def cpu getRHS ( x:${DomainKnowledge.abstracttype} \n")
      writer.write(s"                  y:${DomainKnowledge.abstracttype} \n")
      writer.write(s"                  z:${DomainKnowledge.abstracttype} ) :  ${DomainKnowledge.globaldatatype_L2} \n")
      writer.write("{ \n")
      writer.write(" return ( ( 3 * ( MATHPI * MATHPI ) ) * ( ( sin ( MATHPI * x ) ) * ( ( sin ( MATHPI * y ) ) * ( sin ( MATHPI * z ) ) ) ) ) \n")
      writer.write("}  \n")
    }
    //   }

    writer.write("def cpu setRHS ( lev:Int ) : Unit  \n")
    writer.write("{ \n")
    writer.write("  loop allpoints level lev order lex block 1 1  \n")
    writer.write(s"      f${hoststr} =  getRHS ( location ) \n")
    // writer.write("      f = ( MESHSIZE * MESHSIZE ) * ( getRHS ( location ) ) \n")   
    writer.write("   next \n")
    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
      writer.write(s" repeat up i ( ${DomainKnowledge.nlevels_L2.get} -1)  \n")

      compstr = ""
      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>"
        writer.write(s"	${restrstr} ( ( i0+1 ) \n")
        writer.write(s"	           f ${compstr} [ ( i0+1 ) ] \n")
        writer.write(s"	           f ${compstr} [i0] \n")
        writer.write(s"	           ( ${restrfactor} ) ) \n")
      }
      writer.write("   next \n")
    }
    writer.write("}  \n")

    writer.write(s"def cpu L2Error ( lev:Int ) :  ${DomainKnowledge.abstracttype} \n")
    writer.write("{ \n")
    if (DomainKnowledge.operator_L1(0)._2(0).equals("Laplacian")) {
      writer.write("    Reduction loop innerpoints level lev order lex block 1 1 \n")
      writer.write(s"       s += ( ${DomainKnowledge.unknown_L1(0)._1}${hoststr} [ lev ] - ( getsolution ( location ) ) ) * ( ${DomainKnowledge.unknown_L1(0)._1}${hoststr} [ lev ] - ( getsolution ( location ) ) )   \n")
      writer.write("   next \n")
    } else
      writer.write("  return 1 \n")
    writer.write("}  \n")

    // Generate Application!

    writer.write(s"def cpu Application ( ) : Unit \n")
    writer.write("{  \n")
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( 0  ${DomainKnowledge.unknown_L1(0)._1} <1> [0] ) \n")
      else if (location.equals("cpu"))
        writer.write(s" setcoeff ( 0  ${DomainKnowledge.unknown_L1(0)._1}[0] ) \n")

    writer.write(s" decl res0 : ${DomainKnowledge.abstracttype} = L2Residual ( 0 ) \n")
    writer.write(s" decl res : ${DomainKnowledge.abstracttype} = res0 \n")
    writer.write(s" decl l2e : ${DomainKnowledge.abstracttype} = res0 \n")
    writer.write(s" decl resold : ${DomainKnowledge.abstracttype} = 0 \n")
    writer.write(s" decl mintiming : ${DomainKnowledge.abstracttype} = 10000 \n")
    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle"))
      writer.write(s" decl FMGtiming : ${DomainKnowledge.abstracttype} = 0 \n")
    writer.write("setRHS ( 0 ) \n")
    writer.write(" print ( 'startingres' res0 ) \n")
    writer.write(" \n")
    var curlev = "0"
    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
      writer.write(s" repeat down j ${DomainKnowledge.nlevels_L2.get}  \n")
      curlev = "j0"
      writer.write("print ( 'FMG level:' j0 ) \n")
      writer.write(" mintiming = 10000 \n")
    }
    writer.write(s" repeat up i ${DomainKnowledge.iters_L3.get}  \n")
    //  writer.write("while ( res > ( 0.001 ) ) \n")  
    writer.write(" resold = res \n")
    writer.write(" starttimer ( ) \n")
    writer.write(s"${DomainKnowledge.cycle_L3.get} ( ${curlev} ) \n")
    writer.write(" stoptimer ( ) \n")
    writer.write(" showtimer ( ) \n")
    writer.write(s"res = L2Residual ( ${curlev} ) \n")
    writer.write(s"l2e = L2Error ( ${curlev} ) \n")
    writer.write("print ( 'Residual:' res 'residual reduction:' (res0/res) 'convergence factor:' (res/resold) 'Error' l2e ) \n")
    writer.write("  next  \n")

    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {

      writer.write(s"	FMGtiming = FMGtiming + mintiming \n")
      //       writer.write(" starttimer ( ) \n")
      compstr = ""
      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>"

        writer.write(s"	if ( j0 > 0 ) { \n")
        writer.write(s"	interpolate( (j0 - 1) \n")
        writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [(j0-1)] \n")
        writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [ j0 ] ) \n")
        writer.write(s"	} else { \n")
        writer.write("print ( 'Finest level FMG' ) \n")
        writer.write(s"	} \n")
      }
      writer.write("  next  \n")
      //       writer.write(" stoptimer ( ) \n")
      //       writer.write(" showtimer ( ) \n")
      //       writer.write(s"	FMGtiming = FMGtiming + timing \n")
      writer.write(s"	mintiming = FMGtiming \n")
    }
    writer.write("}  \n")

    writer.close()
    if (DomainKnowledge.debugmode)
      println("DSL level 4 was generated in " + fname)
  }

  def transformL3toL4(fname : String) {

    val writer = new PrintWriter(new File(fname))

    var location = DomainKnowledge.hardware_HW.get

    var hoststr = ""
    if (location.equals("gpu"))
      hoststr = "_host"

    var compstr = ""
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

    var restrstr = "Restriction"
    var intstr = "interpolation"

    var restrfactor = "0.25"
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      restrfactor = "1.0"

    for (i <- DomainKnowledge.discr_iterations)
      writer.write(i.printtoDSL4 + "\n")

    for (field <- treel2.exaFields)
      writer.write(s"Field ${field.name} < [1, 1, 1], ${field.datatype} >@(all) ( ghostlayers = 1, bcDir = ${field.name.equals(DomainKnowledge.unknown_L1(0)._1)} )\n")

    println("<<<--- STENCILS --->>>")
    for (stencil <- treel2.exaOperators)
      println(stencil)
    println("<<<--- STENCILS --->>>")

    /*
    if (DomainKnowledge.cycle_L3.get.equals("VCycle") || DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGVCycle")) {

      writer.write(s"def cpu ${DomainKnowledge.cycle_L3.get}_0 ( lev:Int  ) : Unit \n")
      writer.write("{ \n")
      writer.write(" if coarsestlevel { \n")
      writer.write("repeat up i ncoarse \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
      writer.write(s"	${DomainKnowledge.smoother_L3.get} ( lev) \n")
      writer.write("  next  \n")
      writer.write("} else \n")
      writer.write("{ \n")
      writer.write("  repeat up i nprae \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
      writer.write(s"	${DomainKnowledge.smoother_L3.get}( lev) \n")
      writer.write("  next  \n")

      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")

      writer.write("	Residual ( lev ) \n")

      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>"
        writer.write(s"	${restrstr} ( ( lev+1 ) \n")
        writer.write(s"	           ${DomainKnowledge.function_L1(0)._1} ${compstr} [ ( lev+1 ) ] \n")
        writer.write(s"	           Res ${compstr} [lev] \n")
        writer.write(s"	           ( ${restrfactor} ) ) \n")
        if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
          writer.write(s"	${restrstr} ( ( lev+1 ) \n")
          writer.write(s"	           ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [ ( lev+1 ) ] \n")
          writer.write(s"	           ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [lev] \n")
          writer.write(s"	           ( ${restrfactor} ) ) \n")
        }
      }
      if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle"))
        writer.write(s"	RHS_FAS ( lev+1 ) \n")
      else {
        compstr = ""
        val setname = location match { case "gpu" => "setcuda" case _ => "set" }
        for (i <- 0 to veclength - 1) {
          if (veclength > 1)
            compstr = s"<${i}>"

          writer.write(s"	${setname}( ( lev+1 ) \n")
          writer.write(s"	    ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [ ( lev + 1 ) ] \n")
          writer.write("	     0) \n")
        }
      }

      writer.write(s"	${DomainKnowledge.cycle_L3.get} ( lev+1 ) \n")

      compstr = ""
      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>"

      if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
        writer.write(s"	${restrstr} ( ( lev+1 ) \n")
        writer.write(s"	           Res ${compstr} [ ( lev+1 ) ]  \n")
        writer.write(s"	           ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [lev] \n")
        writer.write(s"	           ( ${restrfactor} ) ) \n")
      }
      }
      
      if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle"))
         writer.write(s"	corr_FAS ( lev+1 ) \n")
        
      compstr = ""
      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>"
        
        writer.write(s"	${intstr}( lev \n")
        writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [lev] \n")
        writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [ (lev+1) ] ) \n")

      }

      writer.write("    repeat up i npost \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
      writer.write(s"		${DomainKnowledge.smoother_L3.get} ( lev ) \n")
      writer.write("  next  \n")
      writer.write("} \n")
      writer.write("} \n")
    }

    writer.write("\n")

    writer.write(s"def ${location} Residual ( lev:Int ) : Unit  \n")
    writer.write("{ \n")    
    
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
      else if (location.equals("cpu"))  
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}[lev] ) \n")
        
    writer.write("loop innerpoints level lev order lex block 1 1  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
    writer.write(s"  Res = ${DomainKnowledge.function_L1(0)._1} [ lev ] - (${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] )  \n")
    writer.write("next \n")
    writer.write("}  \n")

    writer.write("\n")

    if (DomainKnowledge.cycle_L3.get.equals("FASVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
      writer.write("\n")

      writer.write(s"def ${location} RHS_FAS ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write("loop innerpoints level lev order lex block 1 1  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      writer.write(s"  ${DomainKnowledge.function_L1(0)._1} = ${DomainKnowledge.function_L1(0)._1} [ lev ] + (${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] )  \n")
      writer.write(s"  Res = ${DomainKnowledge.unknown_L1(0)._1} [ lev ]  \n")
      writer.write("next \n")
      writer.write("}  \n")

      writer.write("\n")

      writer.write(s"def ${location} corr_FAS ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write("loop innerpoints level lev order lex block 1 1  \n")
      writer.write(s"  ${DomainKnowledge.unknown_L1(0)._1}  = ${DomainKnowledge.unknown_L1(0)._1} [ lev ] - Res [ lev ] \n")
      writer.write("next \n")
      writer.write("}  \n")

      writer.write("\n")

    }
    
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) {
      writer.write(s"def ${location} setcoeff ( lev:Int \n")
      writer.write("          arr:Array) : Unit  \n")
      writer.write("{ \n")
      writer.write(s" decl sigma : ${DomainKnowledge.abstracttype} = (0.1) \n")
      writer.write(s" decl solim : ${DomainKnowledge.abstracttype} = 1 \n")
      writer.write(" decl kdiff : Int = 5 \n")
      writer.write(s" decl kdiffs : ${DomainKnowledge.abstracttype} = (kdiff*sigma) \n")
      writer.write(s" decl sigmakdiffinv : ${DomainKnowledge.abstracttype} = ( ( 1.0 ) / ( kdiffs * kdiffs) ) \n")
      writer.write("  loop innerpoints level arr order lex block 1 1  \n")
      writer.write(s" solim = arr | Im \n")
      writer.write(s" decl sols : ${DomainKnowledge.abstracttype} = ( ( solim * solim ) * sigmakdiffinv ) \n")
      writer.write(s" decl denominv : ${DomainKnowledge.abstracttype} = (( 1.0 ) / ( (1.0) + sols)) \n")
        writer.write("      coeff =  ( ( cos(sigma))  * denominv ) | Re  \n") // real
        writer.write("      coeff =  ( ( sin ( sigma )) * denominv ) | Im    \n") // imag
      writer.write("next  \n")
      writer.write("}  \n")

      writer.write("\n")
      }
      
    if (location.equals("gpu")) {
      writer.write(s"def cpu L2Residual ( lev:Int ) : ${DomainKnowledge.abstracttype} \n")
      writer.write("{ \n")
      writer.write(" Residual ( lev ) \n")
      writer.write(" sqr ( lev  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) {
        writer.write("       Res <0> [lev ] ) \n")
      } else 
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

      writer.write(s"def ${location} L2Residual ( lev:Int ) : ${DomainKnowledge.abstracttype} \n")
      writer.write("{ \n")
      writer.write("    Reduction loop innerpoints level lev order lex block 1 1 \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      if (veclength > 1)
        writer.write(s"        s += (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) * (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) | Vec2 \n")
      else
        writer.write(s"        s += (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) * (${DomainKnowledge.function_L1(0)._1} [ lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ]) \n")
      writer.write("    next  \n")
      writer.write("}  \n")
    }

    if (DomainKnowledge.smoother_L3.get.equals("GaussSeidel")) {
      writer.write(s"def ${location} ${DomainKnowledge.smoother_L3.get} ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1} lev )  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
        else if (location.equals("cpu"))  
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}[lev] ) \n")
        
      writer.write("    loop innerpoints level lev order rb block 1 1 \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1} = ${DomainKnowledge.unknown_L1(0)._1} [ lev ] + ( ( ( inverse( diag(${DomainKnowledge.operator_L1(0)._1} [ lev ] ) ) ) * omega ) * ( ${DomainKnowledge.function_L1(0)._1} [ lev ] - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] ) ) \n")
      writer.write("    next  \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1} lev )  \n")
      writer.write("}  \n")
    } else if (DomainKnowledge.smoother_L3.get.equals("Jacobi")) {
      writer.write(s"def cpu ${DomainKnowledge.smoother_L3.get} ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write(s" ${DomainKnowledge.smoother_L3.get}_1 ( lev ) \n")
      writer.write(s" ${DomainKnowledge.smoother_L3.get}_2 ( lev ) \n")
      writer.write("}  \n")

      writer.write(s"def ${location} ${DomainKnowledge.smoother_L3.get}_1 ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1} lev )  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1} <1> [lev] ) \n")
        else if (location.equals("cpu"))  
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}[lev] ) \n")
        
      writer.write("    loop innerpoints level lev order lex block 1 1 \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1}_old = ${DomainKnowledge.unknown_L1(0)._1} [ lev ] + ( ( ( inverse( diag(${DomainKnowledge.operator_L1(0)._1} [ lev ] ) ) ) * omega ) * ( ${DomainKnowledge.function_L1(0)._1} [ lev ] - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ lev ] ) ) \n")
      writer.write("    next  \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1}_old lev )  \n")
      writer.write("}  \n")
      
      writer.write(s"def ${location} ${DomainKnowledge.smoother_L3.get}_2 ( lev:Int ) : Unit  \n")
      writer.write("{ \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1}_old lev )  \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}_old <1> [lev] ) \n")
        else if (location.equals("cpu"))  
        writer.write(s" setcoeff ( lev  ${DomainKnowledge.unknown_L1(0)._1}_old[lev] ) \n")
        
      writer.write("    loop innerpoints level lev order lex block 1 1 \n")
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
        writer.write(s"  computeStencil ( ${DomainKnowledge.operator_L1(0)._1}  lev  index ) \n")
      writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1} = ${DomainKnowledge.unknown_L1(0)._1}_old [ lev ] + ( ( ( inverse( diag(${DomainKnowledge.operator_L1(0)._1} [ lev ] ) ) ) * omega ) * ( ${DomainKnowledge.function_L1(0)._1} [ lev ] - ${DomainKnowledge.operator_L1(0)._1} [ lev ] * ${DomainKnowledge.unknown_L1(0)._1}_old [ lev ] ) ) \n")
      writer.write("    next  \n")
      writer.write(s"treatBoundary( ${DomainKnowledge.unknown_L1(0)._1} lev )  \n")
      writer.write("}  \n")

    }

    writer.write("\n")

    writer.write(s"def ${location} ${restrstr} ( lev:Int \n")
    writer.write("               coarse:Array \n")
    writer.write("               fine:Array  \n")
    writer.write(s"               factor:${DomainKnowledge.abstracttype}) : Unit  \n")
    writer.write("{ \n")
    writer.write("    loop innerpoints level coarse order lex block 1 1  \n")
    writer.write("      coarse =  ( factor * ( RestrictionStencil * fine ) ) | ToCoarse  \n")
    writer.write("    next  \n")
    writer.write("}  \n")

    writer.write("\n")

    writer.write(s"def ${location} ${intstr}( lev:Int \n")
    writer.write("                     uf:Array  \n")
    writer.write("                     uc:Array ) : Unit \n")
    writer.write("{ \n")
    writer.write("    loop innerpoints level uf order lex block 1 1  \n")
    writer.write("    uf += RestrictionStencil * uc | ToFine  \n")
    writer.write("    next  \n")
    writer.write("}  \n")

    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
      writer.write(s"def ${location} interpolate ( lev:Int \n")
      writer.write("                     uf:Array  \n")
      writer.write("                     uc:Array ) : Unit \n")
      writer.write("{ \n")
      writer.write("    loop innerpoints level uf order lex block 1 1  \n")
      writer.write("    uf = RestrictionStencil * uc | ToFine  \n")
      writer.write("    next  \n")
      writer.write("}  \n")
    }
    writer.write("\n")

    var setfunclistloc: ListBuffer[String] = ListBuffer(s"${location}")
    if (location.equals("gpu"))
      setfunclistloc += "cpu"

    for (n <- setfunclistloc) {
      val setname = n match { case "gpu" => "setcuda" case _ => "set" }
      writer.write(s"def ${n} ${setname} ( lev:Int \n")
      writer.write("          arr:Array  \n")
      writer.write("          value:Int) : Unit  \n")
      writer.write("{ \n")
      writer.write("  loop allpoints level arr order lex block 1 1  \n")
      writer.write("      arr = value    \n")
      writer.write("next  \n")
      writer.write("}  \n")

      writer.write("\n")
    }

    writer.write("def cpu setrandom(arr:Array  \n")
    writer.write("               v:Int) : Unit \n")
    writer.write("{ \n")
    writer.write("  loop innerpoints level arr order lex block 1 1  \n")
    writer.write("      arr = random(v)    \n")
    writer.write("  next  \n")
    writer.write("} \n")

    writer.write("\n")

    // println(DomainKnowledge.operator_L1(0)._2)

    // check solution!
//       if (DomainKnowledge.operator_L1(0)._2(0).equals("Laplacian")) {
      if (DomainKnowledge.rule_dim == 2) {
        writer.write(s"    def cpu getsolution ( x:${DomainKnowledge.abstracttype}\n")
        writer.write(s"                  y:${DomainKnowledge.abstracttype} ) :  ${DomainKnowledge.globaldatatype_L2} \n")
        writer.write("{ \n")
        if (DomainKnowledge.pdebc_L1.get._2.equals("zero"))
          writer.write("  return ( sin ( MATHPI * x ) ) * ( sin ( MATHPI * y ) )\n")
        else if (DomainKnowledge.pdebc_L1.get._2.equals("dn"))
          writer.write("  return ( cos ( MATHPI * x ) ) * ( cos ( MATHPI * y ) )\n")
        writer.write("}  \n")
         
        writer.write(s"def cpu getRHS ( x:${DomainKnowledge.abstracttype} \n")
        writer.write(s"                  y:${DomainKnowledge.abstracttype} ) :  ${DomainKnowledge.globaldatatype_L2}  \n")
        writer.write("{ \n")
        if (DomainKnowledge.pdebc_L1.get._2.equals("zero"))
          writer.write(" return ( ( 2 * ( MATHPI * MATHPI ) ) * ( ( sin ( MATHPI * x ) ) * ( sin ( MATHPI * y ) ) ) ) \n")
        else if (DomainKnowledge.pdebc_L1.get._2.equals("dn"))
          writer.write(" return ( ( 2 * ( MATHPI * MATHPI ) ) * ( ( cos ( MATHPI * x ) ) * ( cos ( MATHPI * y ) ) ) ) \n")
        writer.write("}  \n")
      } else if (DomainKnowledge.rule_dim == 3) {
        writer.write(s"    def cpu getsolution ( x:${DomainKnowledge.abstracttype}\n")
        writer.write(s"                  y:${DomainKnowledge.abstracttype} \n")
        writer.write(s"                  z:${DomainKnowledge.abstracttype} ) :  ${DomainKnowledge.globaldatatype_L2} \n")
        writer.write("{ \n")
        writer.write("  return ( sin ( MATHPI * x ) ) * ( ( sin ( MATHPI * y ) ) * ( sin ( MATHPI * z ) ) )\n")
        writer.write("}  \n")

        writer.write(s"def cpu getRHS ( x:${DomainKnowledge.abstracttype} \n")
        writer.write(s"                  y:${DomainKnowledge.abstracttype} \n")
        writer.write(s"                  z:${DomainKnowledge.abstracttype} ) :  ${DomainKnowledge.globaldatatype_L2} \n")
        writer.write("{ \n")
        writer.write(" return ( ( 3 * ( MATHPI * MATHPI ) ) * ( ( sin ( MATHPI * x ) ) * ( ( sin ( MATHPI * y ) ) * ( sin ( MATHPI * z ) ) ) ) ) \n")
        writer.write("}  \n")
      }
 //   }

    writer.write("def cpu setRHS ( lev:Int ) : Unit  \n")
    writer.write("{ \n")
    writer.write("  loop allpoints level lev order lex block 1 1  \n")
    writer.write(s"      f${hoststr} =  getRHS ( location ) \n")
    // writer.write("      f = ( MESHSIZE * MESHSIZE ) * ( getRHS ( location ) ) \n")   
    writer.write("   next \n")
    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
      writer.write(s" repeat up i ( ${DomainKnowledge.nlevels_L2.get} -1)  \n")
      
        compstr = ""
      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>" 
      writer.write(s"	${restrstr} ( ( i0+1 ) \n")
      writer.write(s"	           f ${compstr} [ ( i0+1 ) ] \n")
      writer.write(s"	           f ${compstr} [i0] \n")
      writer.write(s"	           ( ${restrfactor} ) ) \n")
      }
      writer.write("   next \n")
    }
    writer.write("}  \n")

    writer.write(s"def cpu L2Error ( lev:Int ) :  ${DomainKnowledge.abstracttype} \n")
    writer.write("{ \n")
    if (DomainKnowledge.operator_L1(0)._2(0).equals("Laplacian")) {
      writer.write("    Reduction loop innerpoints level lev order lex block 1 1 \n")
      writer.write(s"       s += ( ${DomainKnowledge.unknown_L1(0)._1}${hoststr} [ lev ] - ( getsolution ( location ) ) ) * ( ${DomainKnowledge.unknown_L1(0)._1}${hoststr} [ lev ] - ( getsolution ( location ) ) )   \n")
      writer.write("   next \n")
    } else
      writer.write("  return 1 \n")
    writer.write("}  \n")

    // Generate Application!

    writer.write(s"def cpu Application ( ) : Unit \n")
    writer.write("{  \n")
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      if (location.equals("gpu"))
        writer.write(s" setcoeff ( 0  ${DomainKnowledge.unknown_L1(0)._1} <1> [0] ) \n")
      else if (location.equals("cpu"))  
        writer.write(s" setcoeff ( 0  ${DomainKnowledge.unknown_L1(0)._1}[0] ) \n")
        
    writer.write(s" decl res0 : ${DomainKnowledge.abstracttype} = L2Residual ( 0 ) \n")
    writer.write(s" decl res : ${DomainKnowledge.abstracttype} = res0 \n")
    writer.write(s" decl l2e : ${DomainKnowledge.abstracttype} = res0 \n")
    writer.write(s" decl resold : ${DomainKnowledge.abstracttype} = 0 \n")
    writer.write(s" decl mintiming : ${DomainKnowledge.abstracttype} = 10000 \n")
    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle"))
      writer.write(s" decl FMGtiming : ${DomainKnowledge.abstracttype} = 0 \n")      
    writer.write("setRHS ( 0 ) \n")
    writer.write(" print ( 'startingres' res0 ) \n")
    writer.write(" \n")
    var curlev = "0"
    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {
      writer.write(s" repeat down j ${DomainKnowledge.nlevels_L2.get}  \n")
      curlev = "j0"
      writer.write("print ( 'FMG level:' j0 ) \n")
      writer.write(" mintiming = 10000 \n")
    }
    writer.write(s" repeat up i ${DomainKnowledge.iters_L3.get}  \n")
    //  writer.write("while ( res > ( 0.001 ) ) \n")  
    writer.write(" resold = res \n")
    writer.write(" starttimer ( ) \n")
    writer.write(s"${DomainKnowledge.cycle_L3.get} ( ${curlev} ) \n")
    writer.write(" stoptimer ( ) \n")
    writer.write(" showtimer ( ) \n")
    writer.write(s"res = L2Residual ( ${curlev} ) \n")
    writer.write(s"l2e = L2Error ( ${curlev} ) \n")
    writer.write("print ( 'Residual:' res 'residual reduction:' (res0/res) 'convergence factor:' (res/resold) 'Error' l2e ) \n")
    writer.write("  next  \n")

    if (DomainKnowledge.cycle_L3.get.equals("FMGVCycle") || DomainKnowledge.cycle_L3.get.equals("FMGFASVCycle")) {

        writer.write(s"	FMGtiming = FMGtiming + mintiming \n")
 //       writer.write(" starttimer ( ) \n")
      compstr = ""
      for (i <- 0 to veclength - 1) {
        if (veclength > 1)
          compstr = s"<${i}>"

        writer.write(s"	if ( j0 > 0 ) { \n")
        writer.write(s"	interpolate( (j0 - 1) \n")
        writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [(j0-1)] \n")
        writer.write(s"	                  ${DomainKnowledge.unknown_L1(0)._1} ${compstr} [ j0 ] ) \n")
        writer.write(s"	} else { \n")
        writer.write("print ( 'Finest level FMG' ) \n")
        writer.write(s"	} \n")
      }
      writer.write("  next  \n")
 //       writer.write(" stoptimer ( ) \n")
 //       writer.write(" showtimer ( ) \n")
 //       writer.write(s"	FMGtiming = FMGtiming + timing \n")
        writer.write(s"	mintiming = FMGtiming \n")
    }
    writer.write("}  \n")
*/
    writer.close()
    if (DomainKnowledge.debugmode)
      println("DSL level 4 was generated in " + fname)
  }
}