package harald.Generate

import java.io._
import scala.collection.mutable.ListBuffer
import harald.dsl.DomainKnowledge
import harald.ast.TreeL2
import exastencils.knowledge._

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

    /*if (DomainKnowledge.cycle_L3.get.equals("VCycle")) {
      for (lev <- 0 to Knowledge.maxLevel) {
        writer.write(s"def cpu ${DomainKnowledge.cycle_L3.get}_$lev (  ) : Unit \n")
        writer.write(s"{ \n")
        if (0 == lev) {
          writer.write(s"repeat up ${Knowledge.mg_cgs_numSteps} \n")
          writer.write(s"	${Knowledge.mg_smoother}_$lev (  ) \n")
          writer.write(s"  next  \n")
        } else {
          writer.write(s"  repeat up ${Knowledge.mg_smoother_numPre} \n")
          writer.write(s"	${Knowledge.mg_smoother}_$lev (  ) \n")
          writer.write(s"  next  \n")
          writer.write(s"	Residual_$lev (  ) \n")
          if (veclength > 1) {
            for (i <- 0 to veclength - 1) {
              writer.write(s"	${DomainKnowledge.restriction_L3.get}_$lev ( ${DomainKnowledge.function_L1(0)._1}_${i}[ ${lev - 1} ] \n")
              writer.write(s"	           Res_${i}[$lev]) \n")
            }
          } else {
            writer.write(s"	${DomainKnowledge.restriction_L3.get}_$lev (  ) \n")
          }
          val setname = location match { case "gpu" => "setcuda" case _ => "set" }
          writer.write(s"	${setname}_${lev - 1} ( 0 ) \n")
          writer.write(s"	${DomainKnowledge.cycle_L3.get}_${lev - 1} (  ) \n")
          writer.write(s"	${DomainKnowledge.interpolation_L3.get}_$lev (  ) \n")
          writer.write(s"  repeat up ${Knowledge.mg_smoother_numPost} \n")
          writer.write(s"		${Knowledge.mg_smoother}_$lev (  ) \n")
          writer.write(s"  next  \n")
        }
        writer.write(s"} \n")
      }
    }*/

    writer.write(s"\n")

    for (lev <- 0 to Knowledge.maxLevel) {
      writer.write(s"def ${location} Residual_$lev (  ) : Unit  \n")
      writer.write(s"{ \n")
      // COMM_HACK
      writer.write(s"  exchsolData_$lev ( 0 )  \n")
      writer.write(s"loop innerpoints level $lev order lex block 1 1  \n")
      writer.write(s"  Res@$lev = ${DomainKnowledge.function_L1(0)._1} [ $lev ] - (${DomainKnowledge.operator_L1(0)._1} [ $lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ $lev ] )  \n")
      writer.write(s"next \n")
      writer.write(s"}  \n")

      writer.write(s"\n")
    }

    /* DISABLED MG FUNCTION      
     if (location.equals("gpu")) {
      writer.write(s"def cpu L2Residual ( lev:Int ) : Double \n")
      writer.write(s"{ \n")
      writer.write(s" Residual ( lev ) \n")
      writer.write(s" sqr ( lev  \n")
      writer.write(s"       Res [lev ] ) \n")
      writer.write(s"return fasterReduce ( Res ) \n")
      writer.write(s"}  \n")

      writer.write(s"\n")

      writer.write(s"def ${location} sqr ( lev:Int \n")
      writer.write(s"          arr:Array) : Unit  \n")
      writer.write(s"{ \n")
      writer.write(s"  loop allpoints level arr order lex block 1 1  \n")
      writer.write(s"      arr = arr * arr    \n")
      writer.write(s"next  \n")
      writer.write(s"}  \n")

      writer.write(s"\n")

    } else {

      for (lev <- 0 to Knowledge.maxLevel) {
        writer.write(s"def ${location} L2Residual_$lev (  ) : Double \n")
        writer.write(s"{ \n")
        // COMM_HACK
        writer.write(s"  exchsolData_$lev ( 0 )  \n")
        writer.write(s"    Reduction loop innerpoints level $lev order lex block 1 1 \n")
        writer.write(s"        s += (${DomainKnowledge.function_L1(0)._1} [ $lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ $lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ $lev ]) * (${DomainKnowledge.function_L1(0)._1} [ $lev ]  - ${DomainKnowledge.operator_L1(0)._1} [ $lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ $lev ]) \n")
        writer.write(s"    next  \n")
        writer.write(s"}  \n")
      }
    }*/

    if (SmootherType.GS == Knowledge.mg_smoother) {
      for (lev <- 0 to Knowledge.maxLevel) {
        writer.write(s"def ${location} ${Knowledge.mg_smoother}_$lev (  ) : Unit  \n")
        writer.write(s"{ \n")
        // COMM_HACK
        writer.write(s"  exchsolData_$lev ( 0 )  \n")
        writer.write(s"    loop innerpoints level $lev order lex block 1 1 \n")
        writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1}@$lev = ${DomainKnowledge.unknown_L1(0)._1} [ $lev ] + ( ( ( inverse( diag(${DomainKnowledge.operator_L1(0)._1} [ $lev ] ) ) ) * ${Knowledge.mg_smoother_omega} ) * ( ${DomainKnowledge.function_L1(0)._1} [ $lev ] - ${DomainKnowledge.operator_L1(0)._1} [ $lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ $lev ] ) ) \n")
        writer.write(s"    next  \n")
        writer.write(s"}  \n")
      }
    }
    if (SmootherType.RBGS == Knowledge.mg_smoother) {
      for (lev <- 0 to Knowledge.maxLevel) {
        writer.write(s"def ${location} ${Knowledge.mg_smoother}_$lev (  ) : Unit  \n")
        writer.write(s"{ \n")
        // COMM_HACK
        writer.write(s"  exchsolData_$lev ( 0 )  \n")
        writer.write(s"    loop innerpoints level $lev order rb block 1 1 \n")
        writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1}@$lev = ${DomainKnowledge.unknown_L1(0)._1} [ $lev ] + ( ( ( inverse( diag(${DomainKnowledge.operator_L1(0)._1} [ $lev ] ) ) ) * ${Knowledge.mg_smoother_omega} ) * ( ${DomainKnowledge.function_L1(0)._1} [ $lev ] - ${DomainKnowledge.operator_L1(0)._1} [ $lev ] * ${DomainKnowledge.unknown_L1(0)._1} [ $lev ] ) ) \n")
        writer.write(s"    next  \n")
        writer.write(s"}  \n")
      }
    }

    writer.write(s"\n")

    for (lev <- 1 to Knowledge.maxLevel) {
      // COMM_HACK
      writer.write(s"def ${location} ${DomainKnowledge.restriction_L3.get}_$lev (  ) : Unit \n")
      writer.write(s"{ \n")
      // COMM_HACK
      writer.write(s"  exchresData_$lev ( 0 )  \n")
      writer.write(s"    loop innerpoints level ${lev - 1} order lex block 1 1  \n")
      // COMM_HACK
      //writer.write(s"      coarse =  RestrictionStencil * fine | ToCoarse  \n")
      writer.write(s"      ${DomainKnowledge.function_L1(0)._1}@${lev - 1} =  RestrictionStencil * Res [ $lev ] | ToCoarse  \n")
      writer.write(s"    next  \n")
      writer.write(s"}  \n")
    }

    writer.write(s"\n")

    for (lev <- 1 to Knowledge.maxLevel) {
      // COMM_HACK
      writer.write(s"def ${location} ${DomainKnowledge.interpolation_L3.get}_$lev (  ) : Unit \n")
      writer.write(s"{ \n")
      // COMM_HACK
      writer.write(s"  exchsolData_${lev - 1} ( 0 )  \n")
      writer.write(s"    loop innerpoints level $lev order lex block 1 1  \n")
      writer.write(s"    ${DomainKnowledge.unknown_L1(0)._1}@$lev += CorrectionStencil * ${DomainKnowledge.unknown_L1(0)._1} [ ${lev - 1} ] | ToFine  \n")
      writer.write(s"    next  \n")
      writer.write(s"}  \n")
    }

    writer.write(s"\n")

    var setfunclistloc : ListBuffer[String] = ListBuffer(s"${location}")
    if (location.equals("gpu"))
      setfunclistloc += "cpu"

    for (n <- setfunclistloc) {
      for (lev <- 0 to Knowledge.maxLevel) {
        val setname = n match { case "gpu" => "setcuda" case _ => "set" }
        writer.write(s"def ${n} ${setname}_$lev ( value:Int ) : Unit  \n")
        writer.write(s"{ \n")
        writer.write(s"  loop allpoints level $lev order lex block 1 1  \n")
        writer.write(s"      ${DomainKnowledge.unknown_L1(0)._1}@$lev = value    \n")
        writer.write(s"next  \n")
        writer.write(s"}  \n")
      }

      writer.write(s"\n")
    }

    // COMM_HACK
    //    writer.write(s"def cpu setrandom(arr:Container \n")
    //    writer.write(s"               v:Int) : Unit \n")
    //    writer.write(s"{ \n")
    //    writer.write(s"  loop innerpoints level arr order lex block 1 1  \n")
    //    writer.write(s"      arr = random(v)    \n")
    //    writer.write(s"  next  \n")
    //    writer.write(s"} \n")
    //
    //    writer.write(s"\n")

    // COMM_HACK
    writer.write(s"def cpu Application ( ) : Unit \n")
    writer.write(s"{  \n")
    writer.write(s"	Residual_${Knowledge.maxLevel} (  ) \n")
    writer.write(s" decl res0 : Double = getGlobalResidual_${Knowledge.maxLevel} (  ) \n")
    writer.write(s" decl res : Double = res0 \n")
    writer.write(s" decl resold : Double = 0 \n")
    writer.write(s" print ( 'startingres' res0 ) \n")
    writer.write(s" repeat up 10 \n")
    writer.write(s" resold = res \n")
    writer.write(s"VCycle_${Knowledge.maxLevel} (  ) \n")
    writer.write(s"	Residual_${Knowledge.maxLevel} (  ) \n")
    writer.write(s"res = getGlobalResidual_${Knowledge.maxLevel} (  ) \n")
    writer.write(s"print ( 'Residual:' res 'residual reduction:' (res0/res) 'convergence factor:' (res/resold) ) \n")
    writer.write(s"  next  \n")
    writer.write(s"}  \n")

    writer.close()
    println("DSL level 4 was generated in " + fname)
  }

}