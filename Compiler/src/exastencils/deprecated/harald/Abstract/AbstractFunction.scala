package exastencils.deprecated.harald.Abstract

import scala.collection.mutable.ListBuffer

import exastencils.deprecated.harald.Impl._
import exastencils.deprecated.harald.ast._
import exastencils.deprecated.harald.dsl._

case class AbstractFunction(fname : String, location : String, rettype : String, paramlist : List[Param], stmts : List[AbstractStatement]) {

  def transform(tree : TreeL2) : ListBuffer[(String, ImplFunction)] = {
    var name : String = fname
    /*    if (fname.equals(DomainKnowledge.smoother_L3.get))
          name = "smoother"
        else
          name = fname
    */
    var palist : ListBuffer[ParameterInfo] = ListBuffer()
    for (p <- paramlist)
      palist += new ParameterInfo(p.name, DomainKnowledge.transform_datatype_cpp(p.dtype))
    var stlist : ListBuffer[ImplStatement] = ListBuffer()
    var varlist : ListBuffer[String] = ListBuffer()

    for (st <- stmts) {
      varlist ++= st.getvariables
      stlist ++= st.transform(palist)
      //      if (fname.equals("setcuda"))
      //        println(st + "\n" + stlist(0).toString_cuda)

    }

    var m : Map[String, Int] = Map()
    for (v <- varlist)
      m += v -> (m.getOrElse(v, 0) + 1)

    var retfunc : ListBuffer[(String, ImplFunction)] = ListBuffer()


    if (location.equals("cpu")) {
      retfunc += name -> new ImplFunction(fname, DomainKnowledge.transform_datatype_cpp(rettype), palist, stlist, m, "cpu")

    } else if (location.equals("gpu")) {

      //  println(fname)

      var stcpulist : ListBuffer[ImplStatement] = ListBuffer()
      //         for (st <- stmts)
      //           if (st.isInstanceOf[Loop] && !st.isInstanceOf[Reduction])
      //             stcpulist ++= st.transform(palist) 

      // Step 1: define CPU function that calls GPU kernel

      //var hs = m.getOrElse("solution", 0)
      var sizestr = ""

      var vecstr = ""
      if (DomainKnowledge.vectorentries > 1)
        vecstr = "0"

      if (paramlist.length == 1)
        sizestr = s"${ DomainKnowledge.unknown_L1(0)._1 }${ vecstr }" + "[lev]"
      else
        sizestr = paramlist(1).name

      // TODO: block size per function!
      if (DomainKnowledge.rule_dim == 2)
        stcpulist += new ImplExternalStatement(s"dim3 dimblock(${ DomainKnowledge.CUDA_BLOCKSIZE(0) },${ DomainKnowledge.CUDA_BLOCKSIZE(1) });\n")
      else if (DomainKnowledge.rule_dim == 3)
        stcpulist += new ImplExternalStatement(s"dim3 dimblock(${ sizestr }.x3_,1);\n")


      var cpuargs : ListBuffer[ImplExpression] = ListBuffer()
      var palistgpu : ListBuffer[ParameterInfo] = ListBuffer()

      if (DomainKnowledge.rule_dim == 2)
        stcpulist += new ImplExternalStatement(s"dim3 dimgrid(((${ sizestr }.x1_)+dimblock.x-1)/dimblock.x,((${ sizestr }.x2_)+dimblock.y-1)/dimblock.y);\n")
      else if (DomainKnowledge.rule_dim == 3)
        stcpulist += new ImplExternalStatement(s"dim3 dimgrid(${ sizestr }.x1_,${ sizestr }.x2_);\n")

      if (paramlist.length == 1) {

        if (DomainKnowledge.stenciltype.equals("nonlinear")) {
          palistgpu += new ParameterInfo("lev", "int")
          cpuargs += new ImplValueExpr[String]("lev")
        }

        for (i <- 0 to DomainKnowledge.rule_dim() - 1)
          cpuargs += new ImplValueExpr[String](s"${ sizestr }.x${ i + 1 }_")

        for (i <- 0 to DomainKnowledge.rule_dim() - 1)
          palistgpu += new ParameterInfo(s"s${ i + 1 }", "int")

      }


      for (m1 <- m) {

        var found = false
        //          println(m1._1)

        for (e <- tree.Fields)
          if (e.name.equals(m1._1)) {
            found = true

            for (i <- 0 to e.veclength - 1) {
              var vecstr = ""
              if (e.veclength > 1)
                vecstr = s"${ i }"

              cpuargs += new ImplValueExpr[String](s"${ m1._1 }${ vecstr }[lev].begin()")

              palistgpu += new ParameterInfo(s"${ m1._1 }${ vecstr }", s"${ DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2) }*")
            }
          }

        for (e <- tree.Stencils)
          if (e.name.equals(m1._1)) {
            found = true
            cpuargs += new ImplValueExpr[String](m1._1 + "[lev].begin()")

            palistgpu += new ParameterInfo(s"${ m1._1 }", s"${ DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2) }*")
          }
      }

      if ((DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) && !name.equals("setcoeff")) {
        cpuargs += new ImplValueExpr[String]("coeff0[lev].begin()")
        cpuargs += new ImplValueExpr[String]("coeff1[lev].begin()")

        palistgpu += new ParameterInfo(s"coeff0", s"${ DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2) }*")
        palistgpu += new ParameterInfo(s"coeff1", s"${ DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2) }*")

      }

      var first_array = 0

      for (i <- 1 to paramlist.length - 1) {
        //println(paramlist(i).name)
        if (paramlist(i).dtype.equals("Array")) {
          for (j <- 0 to DomainKnowledge.rule_dim() - 1)
            cpuargs += new ImplValueExpr[String](s"${ paramlist(i).name }.x${ j + 1 }_")
          cpuargs += new ImplValueExpr[String](paramlist(i).name + ".begin()")

          if (first_array == 0) {
            first_array += 1
            for (j <- 0 to DomainKnowledge.rule_dim() - 1)
              palistgpu += new ParameterInfo(s"s${ j + 1 }", "int")
          } else {
            for (j <- 0 to DomainKnowledge.rule_dim() - 1)
              palistgpu += new ParameterInfo(s"s${ j + 1 }_${ first_array }", "int")
            first_array += 1
          }

          palistgpu += new ParameterInfo(s"${ paramlist(i).name }", s"${ DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2) }*")

        } else {
          cpuargs += new ImplValueExpr[String](s"${ paramlist(i).name }")

          palistgpu += new ParameterInfo(paramlist(i).name, DomainKnowledge.transform_datatype_cuda(paramlist(i).dtype))
        }
      }

      // additional argument for color in rb GS
      if (fname.equals("GaussSeidel")) {
        var cpuargs_call1 : ListBuffer[ImplExpression] = ListBuffer()
        cpuargs_call1 ++= cpuargs
        cpuargs_call1 += new ImplValueExpr[String]("0")
        stcpulist += new ImplPcall("", fname + "cuda", cpuargs_call1)
        var cpuargs_call2 : ListBuffer[ImplExpression] = ListBuffer()
        cpuargs_call2 ++= cpuargs
        cpuargs_call2 += new ImplValueExpr[String]("1")
        stcpulist += new ImplPcall("", fname + "cuda", cpuargs_call2)
      } else
        stcpulist += new ImplPcall("", fname + "cuda", cpuargs)

      var palist2 : ListBuffer[ParameterInfo] = ListBuffer()
      for (p <- paramlist)
        palist2 += new ParameterInfo(p.name, DomainKnowledge.transform_datatype_cpp_cuda(p.dtype))

      retfunc += name -> new ImplFunction(fname, DomainKnowledge.transform_datatype_cpp(rettype), palist2, stcpulist, m, "cpu")

      //  std::cout << "Res" << fasterReduce (Res[lev].begin(), solution[lev].x1_*solution[lev].x2_, f[lev].begin()) << std::endl;

      //println(varlist + " " + m)
      /*
        for (i <- 0 to DomainKnowledge.rule_dim() - 1)
          palistgpu += new ParameterInfo(s"s${i}", "int")
       for (p <- paramlist)
          if (!p.name.equals("lev"))
           palistgpu += new ParameterInfo(p.name, TransformL2.transform_datatype_cuda(p.dtype))
        for (m1 <- m) {
          var found = false
          for (p <- paramlist)
            if (p.name.equals(m1._1))
              found = true
          if (!found)    
           palistgpu += new ParameterInfo(s"${m1._1}", "double*")
        }
        */



      if (!rettype.equals("Unit"))
        palistgpu += new ParameterInfo("ret", DomainKnowledge.transform_datatype_cuda(rettype) + "*")

      if (fname.equals("GaussSeidel")) {
        palistgpu += new ParameterInfo("red_black", DomainKnowledge.transform_datatype_cuda("Int"))
        retfunc += s"${ name }cuda" -> new ImplFunction(fname + "cuda", "void", palistgpu, stlist, m, "gpu")
      } else
        retfunc += s"${ name }cuda" -> new ImplFunction(fname + "cuda", "void", palistgpu, stlist, m, "gpu")
    }

    return retfunc
  }
}
