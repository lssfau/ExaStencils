package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.ast.TreeManager
import harald.expert.StencilGenerator

  class ImplStatement() extends ImplBase {
    override def toString = ""
    def contains_modifier(s : String) = false
  }

  case class ImplExternalStatement(s: String) extends ImplStatement {
    override def toString = s
    override def toString_cpp: String = s
    override def costs(para: String): Map[Int,String] = {
      return CostInfo.get(para).getOrElse(Map())
    }
  }


  class ImplCommunication(fname: String, loc: String) extends ImplStatement {
    
    override def toString_cpp: String = {
      return s"transfer(${fname},${loc});\n"
    }
    override def costs(para: String): Map[Int,String] = {
      return CostInfo.get(para).getOrElse(Map())
    }
  }


  class ImplReductionStatement(s: ImplStatement) extends ImplStatement {
    override def toString_cpp: String = s"${DomainKnowledge.datatype_L2.getOrElse("double")} s = 0; \n " + s.toString_cpp + "return s;"
    override def costs(para: String) : Map[Int,String] = s.costs(para)
  }

  class Implforloop(loopvar: ListBuffer[ParameterInfo], start: ListBuffer[ImplExpression], stop: ListBuffer[ImplExpression], stepsize: ListBuffer[Int], runningorder: String, blocksize: Int, body: ListBuffer[ImplStatement]) extends ImplStatement {
    override def toString_cpp: String = {
      var s: String = ""
      for (b <- body)
        s += b.toString_cpp
      var sloops: String = "{"

      if (runningorder.equals("rb")) {
        // multicolor: int offset = ( i0 % 2 == 0 ? 1 : 2 ); für alle += 2 erster index 1,2 dann offset2 = ( i % 2 == offs2 ? 1 : 2 ); offset3 = ( j % 2 == offs3 ? 2 : 1 );
        for (i <- 0 to start.length - 1)
          if (stepsize(i) == 0)
            sloops += s"${loopvar(0).dtype} ${loopvar(0).name}${i} = ${stop(i).toString_cpp}; \n"

        for (i <- 0 to start.length - 1) {
          var steps = ""
          if (i == start.length - 1)
            steps = s"${loopvar(0).name}${i}+=2"
          else
            steps = s"++${loopvar(0).name}" + i

          if (!(stepsize(i) == 0))
            if (i == start.length - 1) {
              var idx = ""
              if (DomainKnowledge.rule_dim() == 2)
                idx = s"${loopvar(0).name}0"
              else
                idx = s"(${loopvar(0).name}0+${loopvar(0).name}1)"

              sloops += s"{ int offset = ( ${idx} % 2 == 0 ? 1 : 2 ); \n for ( ${loopvar(0).dtype} " + s"${loopvar(0).name}" + i + s" = offset; ${loopvar(0).name}" + i + " < " + stop(i).toString_cpp + ";" + steps + ")\n"
            } else
              sloops += "for ( int " + "i" + i + " = " + start(i).toString_cpp + "; i" + i + " < " + stop(i).toString_cpp + ";" + steps + ")\n"
        }
        sloops += " { \n" + s + "}}\n"

        for (i <- 0 to start.length - 1) {
          var steps = ""
          if (i == start.length - 1)
            steps = s"${loopvar(0).name}${i}+=2"
          else
            steps = s"++${loopvar(0).name}" + i

          if (!(stepsize(i) == 0))
            if (i == start.length - 1) {
              var idx = ""
              if (DomainKnowledge.rule_dim() == 2)
                idx = s"${loopvar(0).name}0"
              else
                idx = s"(${loopvar(0).name}0+${loopvar(0).name}1)"

              sloops += s"{ int offset = ( ${idx} % 2 == 0 ? 2 : 1 ); \n for ( ${loopvar(0).dtype} " + s"${loopvar(0).name}" + i + s" = offset; ${loopvar(0).name}" + i + " < " + stop(i).toString_cpp + ";" + steps + ")\n"
            } else
               sloops += s"for ( ${loopvar(0).dtype} " + s"${loopvar(0).name}" + i + " = " + start(i).toString_cpp + s"; ${loopvar(0).name}" + i + " < " + stop(i).toString_cpp + ";" + steps + ")\n"
        }
        sloops += " { \n" + s + "}}\n"

      } else { // lex

        for (i <- 0 to start.length - 1)
          if (stepsize(i) == 0)
            sloops += s"${loopvar(0).dtype} ${loopvar(0).name}${i} = ${stop(i).toString_cpp}; \n"

        for (i <- 0 to start.length - 1) {
          //        sloops += "for ( " + "i" + i + " <-" + start(i).toString + " to " + stop(i).toString + ")" 
          var steps = ""
          if (stepsize(i) == 1)
            steps = s"++${loopvar(0).name}" + i
          else if (stepsize(i) == -1)
            steps = s"--${loopvar(0).name}" + i
          else if (stepsize(i) > 0)
            steps = s"${loopvar(0).name}${i}+=${stepsize(i)}"
          else
            steps = s"${loopvar(0).name}${i}-=${stepsize(i)}"

          if (!(stepsize(i) == 0))
            if (stepsize(i) > 0)
              sloops += s"for ( ${loopvar(0).dtype} " + s"${loopvar(0).name}" + i + " = " + start(i).toString_cpp + s"; ${loopvar(0).name}" + i + " < " + stop(i).toString_cpp + ";" + steps + ")\n"
            else
              sloops += s"for ( ${loopvar(0).dtype} ${loopvar(0).name}${i} = ${stop(i).toString_cpp} - 1; ${loopvar(0).name}${i} >= ${start(i).toString_cpp}; ${steps}) \n"
        }
        sloops += " { \n" + s + "}"
      }

      return sloops + "} \n"
    }

    override def toString_cuda: String = {
      var s: String = ""

      if (DomainKnowledge.rule_dim() == 2) {
        s += "unsigned int i1 = blockIdx.x*blockDim.x + threadIdx.x;\n"
        s += "unsigned int i2 = blockIdx.y*blockDim.y + threadIdx.y;\n"
                
        s += s"unsigned int global_idx = ${IdxKnowledge.mapidxToLinear(ListBuffer("i1", "i2"), ListBuffer("s1", "s2"))};\n"
        for (b <- body) {
          if (b.contains_modifier("ToCoarse")) {
            var lb : ListBuffer[String] = ListBuffer()
            for (i <- 1 to DomainKnowledge.rule_dim())
              lb += DomainKnowledge.rule_mapcoarseTofine("i" + (i).toString)

            s += s"unsigned int global_idx_2 = ${IdxKnowledge.mapidxToLinear(lb, ListBuffer("s1_1", "s2_1"))};\n"
//          println("in cuda loop" + b.toString_cpp)
      
            var curStencil = TreeManager.tree.Stencils(0)    
       for (st <- TreeManager.tree.Stencils)
         if (st.name.equals("RestrictionStencil"))
           curStencil = st
           
            val exprloop : ImplExpression = StencilGenerator.generateStencilConvolutioncuda(9,curStencil,"fine",1, "", "global_idx_2")
        s += s"if (i1 >= ${start(0).toString_cpp} && i2 >= ${start(1).toString_cpp} && i1 < s1 - ${start(0).toString_cpp} && i2 < s2 - ${start(1).toString_cpp}) { \n"
            val statloop = b match {
              case ImplAssigmentStatement(variable,op,expr,mod) => new ImplAssigmentStatement(variable,op,exprloop,mod)  
            }
            s += statloop.toString_cuda
            s += "}\n"

            return s
          }
          if (b.contains_modifier("ToFine")) {
            var lb : ListBuffer[String] = ListBuffer()
            for (i <- 1 to DomainKnowledge.rule_dim())
              lb += DomainKnowledge.rule_mapcoarseTofine("i" + (i).toString)

            s += s"unsigned int global_idx_2 = ${IdxKnowledge.mapidxToLinear(lb, ListBuffer("s1_1", "s2_1"))};\n"
//          println("in cuda loop" + b.toString_cpp)
            val exprloop : ImplExpression = StencilGenerator.generateStencilInterpolationcuda("uc", "i")
        s += s"if (i1 >= ${start(0).toString_cpp} && i2 >= ${start(1).toString_cpp} && i1 < s1 - ${start(0).toString_cpp} && i2 < s2 - ${start(1).toString_cpp}) { \n"
            val statloop = b match {
              case ImplAssigmentStatement(variable,op,expr,mod) => new ImplAssigmentStatement(variable,op,exprloop,mod)  
            }
            s += statloop.toString_cuda
            s += "}\n"

            return s
          }
        }
        
        s += s"if (i1 >= ${start(0).toString_cpp} && i2 >= ${start(1).toString_cpp} && i1 < s1 - ${start(0).toString_cpp} && i2 < s2 - ${start(1).toString_cpp}) { \n"
        if (runningorder.equals("rb"))  
         s += s"if (((i1+i2)%2) == red_black) \n"
         
        for (b <- body)
          s += b.toString_cuda
        s += "}\n"
        return s
      } else
        return ""
    }

    override def calls(lev: Int) {
      for (i <- 0 to (stop(0).evaluate(ListBuffer(new ParameterInfo("", ""))) - start(0).evaluate(ListBuffer(new ParameterInfo("", ""))) - 1))
        for (b <- body)
          b.calls(lev)
    }

    override def costs(para: String): Map[Int,String] = {
      var m : Map[Int,String] = Map()
      for (b <- body)
        m ++= b.costs(para)
      /*
     var s = stop(0).toString_cpp.toInt        
     for (i <- 1 to start.length-1)
       s = s * stop(i).toString_cpp.toInt   
     CostInfo += "*" -> s
					 */

      //   println("loop " + CostInfo)
      //    println("(" + s + ")*" + sc)
      //      println("loopend " )
      return m
      //     return "(" + s + ")*" + sc
    }
  }

  class ImplIfelseStatement(expr: ImplExpression, ifbody: ListBuffer[ImplStatement], elsebody: ListBuffer[ImplStatement]) extends ImplStatement {
    var e: ImplExpression = expr

    override def toString_cpp: String = {
      var sif: String = ""
      for (b <- ifbody)
        sif += b.toString_cpp
      var selse: String = ""
      for (b <- elsebody)
        selse += b.toString_cpp
      return "if ( " + expr.toString_cpp + " ) { \n" + sif + "\n } else { \n" + selse + "} \n"
    }

    override def calls(lev: Int) {
      if (e.evaluate(ListBuffer(new ParameterInfo("lev", "", lev))) == 1)
        for (b <- ifbody)
          b.calls(lev)
      else
        for (b <- elsebody)
          b.calls(lev)
    }

    override def costs(para: String): Map[Int,String] = {

      var m : Map[Int,String] = Map()
      println(e.toString_cpp)
      //     println(e.evaluate(para))

      //     if (e.evaluate(para) == 1)
      for (b <- ifbody)
        //      println(b.toString_cpp)
        m ++= b.costs(para)
      /*     } else {
       for (b <- elsebody) {
//         println(b.toString_cpp)
        s = s + b.costs(para)
       }
      }
			 */
      // println("in" + s)
      return m
      //     DomainKnowledge.CostInfo += "ifelse" -> (DomainKnowledge.CostInfo.getOrElse("ifelse", 0) + 1)    
    }
  }

  case class ImplAssigmentStatement(variable: ImplVariable, op: OperatorInfo, expr: ImplExpression, modifierstring: String = "") extends ImplStatement {
    override def toString_cpp: String = { variable.toString_cpp + op.toString_cpp + expr.toString_cpp + ";" }
    override def toString_cuda: String = variable.toString_cuda + op.toString_cpp + expr.toString_cuda + ";"     
    override def contains_modifier(s : String) : Boolean = {
      if (modifierstring.equals(s))
        return true
      else
        return false    
    }

    override def costs(para: String): Map[Int,String] = {
      CostInfo += "Store" -> variable.costs(para)
      CostInfo += "Load" -> expr.costs(para)

      if (op.equals("+=")) {
        CostInfo += "+" -> Map(1 -> "")
        CostInfo += "Load" -> variable.costs(para)
      }

      //println("assign " + CostInfo)      
      //     return "(" + expr.costs(para) + "+" + variable.costs(para) + ")"

      var m : Map[Int,String] = Map()
      m ++= expr.costs(para)
      m ++= variable.costs(para)
      m ++= CostInfo.get(para).getOrElse(Map())
      return m
    }
  }

  class ImplReturnStatement(expr: ImplExpression) extends ImplStatement {
    override def toString_cpp: String = "return " + expr.toString_cpp + ";\n"
    override def costs(para: String) = expr.costs(para)

  }
/*
  case class ImplDefinition(val p : ParameterInfo, val value: ImplExpression) extends ImplStatement  {

    override def toString_cpp = s"${p.dtype} ${p.name} = ${value.toString} ;\n"
    override def costs(para: String) = value.costs(para)
  }
*/
  class ImplDefinitionStatement(val name: String, val dtype: String, val value: ImplExpression) extends ImplStatement {
    override def toString_cpp: String = s"${dtype} ${name} = ${value.toString_cpp};\n"
    override def costs(para: String): Map[Int,String] = {
      return Map()
    }
  }
  
  class ImplPcall(obj: String, name: String, paramlist: ListBuffer[ImplExpression]) extends ImplStatement {
    override def toString_cpp: String =
      {
        var objs = obj
        if (!objs.equals(""))
          objs = objs + "."

        if (name.equals("print")) {
          if (TreeManager.tree.isinFields(paramlist(0).toString_cpp)) {
            var s: String = DomainKnowledge.rule_idxArray_cpp()
            return "std::cout << " + paramlist(0).toString_cpp + s + " << \" \" ;"
          } else {

            var pstr = "std::cout << "
          for (p <- paramlist)
            pstr += p.toString_cpp + " << \" \" << " 
          pstr += " std::endl; "
            return pstr
          }
        }
        //    return name + " ( " + paramlist.mkString(",") + ");\n"

        var location = ""
        for (f <- TreeManager.tree.Functions)
          if (f._2.name.equals(name))
            location = f._2.location

        var s = ""

        if (location.equals("cpu")) {
          s = objs + name + " ( "
          if (paramlist.length > 0)
            s = s + paramlist(0).toString_cpp
          for (i <- 1 to paramlist.length - 1)
            s = s + "," + paramlist(i).toString_cpp
          return s + ");\n"

        } else {
          s = objs + name + "<<<dimgrid,dimblock>>> ( "
          if (paramlist.length > 0)
            s = s + paramlist(0).toString_cpp
          for (i <- 1 to paramlist.length - 1)
            s = s + "," + paramlist(i).toString_cpp
          return s + ");\n"

        }

      }

    override def calls(lev: Int) {
      // println(name + " " + paramlist.toString + " with " + paramlist(0).evaluate(ListBuffer(new ParameterInfo("lev","",lev))))

      for (f <- TreeManager.tree.Functions) {
        if (f._2.name.equals(name)) {
          if (f._2.location.equals("cpu"))
           f._2.calls(paramlist(0).evaluate(ListBuffer(new ParameterInfo("lev", "", lev))))
          else 
           f._2.calls(0)
          var tempf: Map[Int, Int] = TreeManager.tree.callgraph.get(f._2.name).getOrElse(Map())
          var tempn: Int = 0
          if (f._2.location.equals("cpu"))
            tempn = tempf.get(paramlist(0).evaluate(ListBuffer(new ParameterInfo("lev", "", lev)))).getOrElse(0)
          else  
            tempn = tempf.get(0).getOrElse(0)
          tempf += lev -> (tempn + 1)
          TreeManager.tree.callgraph += f._2.name -> tempf
        }
      }
    }

    override def costs(para: String): Map[Int,String] = {
      // println(name + " " + paramlist.toString)
      /*     if (paramlist.length > 0) {
       println(paramlist(0).evaluate(para))
       val i = paramlist(0).evaluate(para)

     for (f <- Functions)
       if (f._2.name.equals(name)) {
         //println(f._2.params(0).name)
         val fnam = f._2.params(0).name

      for (p <- para)
       if (p.name.equals(fnam))
           p.v = i

        //return f._2.costs(para)
       }
     }
 */
      return Map()
    }

    def evaluate {}

  }
