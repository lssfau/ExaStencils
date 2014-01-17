package harald.Abstract

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.Impl._
import harald.ast.TreeL2
import exastencils.datastructures.ir._

case class AbstractFunction(fname: String, location: String, rettype: String, paramlist: List[Param], stmts: List[AbstractStatement]) {

  def transform(tree: TreeL2): ListBuffer[(String, ImplFunction)] = {
    var name: String = ""
    if (fname.equals(DomainKnowledge.smoother_L3.get))
      name = "smoother"
    else
      name = fname

    var palist: ListBuffer[ParameterInfo] = ListBuffer()
    for (p <- paramlist)
      palist += new ParameterInfo(p.name, DomainKnowledge.transform_datatype_cpp(p.dtype))
    var stlist: ListBuffer[Statement] = ListBuffer()
    var varlist: ListBuffer[String] = ListBuffer()

    for (st <- stmts) {
      varlist ++= st.getvariables
      stlist ++= st.transform(palist)
    }

    var m: Map[String, Int] = Map()
    for (v <- varlist)
      m += v -> (m.getOrElse(v, 0) + 1)

    var retfunc: ListBuffer[(String, ImplFunction)] = ListBuffer()

    if (location.equals("cpu")) {
      retfunc += name -> new ImplFunction(fname, DomainKnowledge.transform_datatype_cpp(rettype), palist, stlist, m, "cpu")

    } else if (location.equals("gpu")) {

      var stcpulist: ListBuffer[Statement] = ListBuffer()
      //         for (st <- stmts)
      //           if (st.isInstanceOf[Loop] && !st.isInstanceOf[Reduction])
      //             stcpulist ++= st.transform(palist) 

      stcpulist += new ImplExternalStatement(s"dim3 dimblock(${DomainKnowledge.CUDA_BLOCKSIZE(0)},${DomainKnowledge.CUDA_BLOCKSIZE(1)});\n")
      //var hs = m.getOrElse("solution", 0)
      var sizestr = ""

      var cpuargs: ListBuffer[Expression] = ListBuffer()
      var palistgpu: ListBuffer[ParameterInfo] = ListBuffer()

      if (paramlist.length == 1) {
        sizestr = "solution" + "[lev]"
        for (i <- 0 to DomainKnowledge.rule_dim() - 1)
          cpuargs += new ImplValueExpr[String](s"${sizestr}.x${i + 1}_")
        for (i <- 0 to DomainKnowledge.rule_dim() - 1)
          palistgpu += new ParameterInfo(s"s${i + 1}", "int")

      } else
        //if (hs == 0)
        sizestr = paramlist(1).name

      stcpulist += new ImplExternalStatement(s"dim3 dimgrid(((${sizestr}.x1_)+dimblock.x-1)/dimblock.x,((${sizestr}.x2_)+dimblock.y-1)/dimblock.y);\n")
      //        cpuargs += new TransformL4.ValueExpr[Int](0)

      for (m1 <- m) {

        var found = false
        //          println(m1._1)

        for (e <- tree.Fields)
          if (e.name.equals(m1._1)) {
            found = true
            cpuargs += new ImplValueExpr[String](m1._1 + "[lev].begin()")

            palistgpu += new ParameterInfo(s"${m1._1}", "double*")
          }

        for (e <- tree.Stencils)
          if (e.name.equals(m1._1)) {
            found = true
            cpuargs += new ImplValueExpr[String](m1._1 + "[0].begin()")

            palistgpu += new ParameterInfo(s"${m1._1}", "double*")
          }
      }

      var first_array = 0

      for (i <- 1 to paramlist.length - 1) {
        println(paramlist(i).name)
        if (paramlist(i).dtype.equals("Array")) {
          for (j <- 0 to DomainKnowledge.rule_dim() - 1)
            cpuargs += new ImplValueExpr[String](s"${paramlist(i).name}.x${j + 1}_")
          cpuargs += new ImplValueExpr[String](paramlist(i).name + ".begin()")

          if (first_array == 0) {
            first_array += 1
            for (j <- 0 to DomainKnowledge.rule_dim() - 1)
              palistgpu += new ParameterInfo(s"s${j + 1}", "int")
          } else {
            for (j <- 0 to DomainKnowledge.rule_dim() - 1)
              palistgpu += new ParameterInfo(s"s${j + 1}_${first_array}", "int")
            first_array += 1
          }

          palistgpu += new ParameterInfo(s"${paramlist(i).name}", "double*")

        } else {
          cpuargs += new ImplValueExpr[String](s"${paramlist(i).name}")

          palistgpu += new ParameterInfo(paramlist(i).name, DomainKnowledge.transform_datatype_cuda(paramlist(i).dtype))
        }
      }

      if (fname.equals("GaussSeidel")) {
        var cpuargs_call1: ListBuffer[Expression] = ListBuffer()
        cpuargs_call1 ++= cpuargs
        cpuargs_call1 += new ImplValueExpr[String]("0")
        stcpulist += new ImplPcall("", fname + "cuda", cpuargs_call1)
        var cpuargs_call2: ListBuffer[Expression] = ListBuffer()
        cpuargs_call2 ++= cpuargs
        cpuargs_call2 += new ImplValueExpr[String]("1")
        stcpulist += new ImplPcall("", fname + "cuda", cpuargs_call2)
      } else
        stcpulist += new ImplPcall("", fname + "cuda", cpuargs)

      var palist2: ListBuffer[ParameterInfo] = ListBuffer()
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
        retfunc += s"${name}cuda" -> new ImplFunction(fname + "cuda", "void", palistgpu, stlist, m, "gpu")
      } else
        retfunc += s"${name}cuda" -> new ImplFunction(fname + "cuda", "void", palistgpu, stlist, m, "gpu")
    }

    return retfunc
  }
}
