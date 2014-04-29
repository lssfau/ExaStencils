package harald.Abstract

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.Impl._
import harald.ast.TreeManager
import harald.expert.StencilGenerator

sealed abstract class AbstractExpression {
  def value(context: Context): String
  def transform(scopeparas: ListBuffer[ParameterInfo], compstr : String, modifier: Option[String], scopetype: String): ImplExpression
  def getvariables: ListBuffer[String] = ListBuffer()
}

case class AbstractBinaryOp(operator: String, left: AbstractExpression, right: AbstractExpression) extends AbstractExpression {
  override def value(context: Context) = left.value(context)
  override def toString = left.toString + " " + operator + " " + right.toString

  override def transform(scopeparas: ListBuffer[ParameterInfo], compstr : String, modifier: Option[String], scopetype: String): ImplExpression = {
    // check for convolution M * v
    if (operator.equals("*"))
      left match {
        case AbstractVariable(id1, comp1, l1) => {
          for (e1 <- TreeManager.tree.Stencils)
            if (e1.name.equals(id1))
              right match {
                case AbstractVariable(id2,comp2, l2) => {
                  var lb: ListBuffer[ImplExpression] = new ListBuffer()
                  lb += new ImplVariable("", id2, comp2,new TypeInfo(id2, 1), l2.transform(scopeparas, compstr, modifier, "argument"), "argument") // TODO

                  if (modifier.getOrElse("").equals("ToCoarse")) {

                    // TODO: implicit assumption: loop index i0,i1,i2
                    if (!DomainKnowledge.use_gpu) {
                      for (i <- 1 to DomainKnowledge.rule_dim())
                        lb += new ImplValueExpr[String](DomainKnowledge.rule_mapcoarseTofine("i" + (i - 1).toString,i-1))

                      //return new functioncall(id1 + s"[0]", "convolve" + e1.length + "P", lb)

                      var memlistS: ListBuffer[ParameterInfo] = ListBuffer()
                      memlistS += new ParameterInfo("fine", DomainKnowledge.ArrayClassName + "<T>&")
                      for (i <- 1 to DomainKnowledge.rule_dim())
                        memlistS += new ParameterInfo(s"2*i${i - 1}", "int")
                       // TODO: implicit assumption: on finest level [0]
                      return StencilGenerator.generateStencilConvolution(id1 + "[0]", e1, memlistS, "")
//                      return StencilGenerator.generateStencilConvolutioncuda(e1.length(0), e1, id1, 0, "", "global_idx", "")
                      //  	                     return new functioncall(id1+s"[${l1}]","convolve" + e1.length + "P", lb) 
                    }
                  } else if (modifier.getOrElse("").equals("ToFine")) {

                    for (i <- 1 to DomainKnowledge.rule_dim())
                      lb += new ImplValueExpr[String]("i" + (i - 1).toString)

                    return new ImplFcall(id1 + "[0]", "interpolate", lb) //ListBuffer(new VariableInfo(id2 , new TypeInfo(id2,1), mapexpression(l2,scopeparas,modifier,"argument").toString, "argument"), new ValueExpr[String]("i0"),new ValueExpr[String]("i1")))
                  } else {

                    if (DomainKnowledge.use_gpu) {

                      var curStencil = TreeManager.tree.Stencils(0)
                      for (st <- TreeManager.tree.Stencils)
                        if (st.name.equals(e1.name))
                          curStencil = st
                        return StencilGenerator.generateStencilConvolutioncuda(e1.length(0), curStencil, id2, 0, "", "global_idx", compstr)
 
                    } else {
                      for (i <- 1 to DomainKnowledge.rule_dim())
                        lb += new ImplValueExpr[String]("i" + (i - 1).toString)

                        /*if ( DomainKnowledge.stenciltype.equals("nonlinear")) 
                          return new ImplFcall(id1 + s"[lev]", "convolve" + e1.length(0) + "P", lb)
                        else */if (e1.sizex > 1)
                          return new ImplFcall(id1 + s"[lev]", "convolve" + e1.length(0) + "P", lb)
                          
                      //  	                         return new functioncall(id1+s"[${l1}]","convolve" + e1.length + "P", lb) 

                      var exprlist : ListBuffer[ImplExpression] = ListBuffer()
                      var c1str = ""
                      var c2str = ""
                          
                      for (c <- 0 to e1.matlength1-1) {
                        if (e1.matlength1 > 1) {
                          c1str = s"${c}"
                          c2str = "_" + c1str
                        }
                      var memlistS: ListBuffer[ParameterInfo] = ListBuffer()
                      memlistS += new ParameterInfo(s"${id2}${c1str}[lev]", DomainKnowledge.ArrayClassName + "<T>&")
                      for (i <- 1 to DomainKnowledge.rule_dim())
                        memlistS += new ParameterInfo(s"i${i - 1}", "int")
                      if ( DomainKnowledge.stenciltype.equals("nonlinear")) 
                        exprlist += StencilGenerator.generateStencilConvolution("", e1, memlistS, "")
                      else
                        exprlist += StencilGenerator.generateStencilConvolution(id1 + s"${compstr}${c2str}[lev]", e1, memlistS, "")
                      }
                      
                     // return new ImplBinaryExpr(exprlist(0), new OperatorInfo("+"), exprlist(1))
                      return recVecStencilgeneration(exprlist)

                    }
                  }
                }
                case _ =>
              }
        }
        case _ =>
      }

    return new ImplBinaryExpr(left.transform(scopeparas, compstr, modifier, scopetype), new OperatorInfo(operator), right.transform(scopeparas, compstr, modifier, scopetype))
  }
  
  def recVecStencilgeneration(exprlist : ListBuffer[ImplExpression]) : ImplExpression = {
    if (exprlist.length == 1)
      return exprlist(0)
    else
      return new ImplBinaryExpr(exprlist.head, new OperatorInfo("+"), recVecStencilgeneration(exprlist.tail))
  }
  
  override def getvariables: ListBuffer[String] = {
    var s1 = left.getvariables
    var s2 = right.getvariables
    for (s <- s2)
      s1 += s
    return s1
  }
}

case class AbstractFCall(fname: String, arglist: List[AbstractExpression]) extends AbstractExpression {
  override def value(context: Context) = "return"
  override def toString = fname + "(" + arglist.mkString(",") + ")"

  override def transform(scopeparas: ListBuffer[ParameterInfo],compstr : String,  modifier: Option[String], scopetype: String): ImplExpression = {
    var args: ListBuffer[ImplExpression] = ListBuffer()
    for (a <- arglist) {
//      println("aha " + a + " " + a.getClass())
      a match {
        case AbstractVariable("location",_,_) => {
          var lb : ListBuffer[String] = ListBuffer()
          lb += "i0"
          for (i <- 1 to DomainKnowledge.rule_dim() - 1) 
            lb += s"i${i}"
          
          val phcoords = IdxKnowledge.mapidxTocoordcast(lb,"lev")
          for (i <- 0 to DomainKnowledge.rule_dim() - 1) {
            var loclist : AbstractExpression = new AbstractVariable(phcoords(i),"",new AbstractLiteral(""))
            args += loclist.transform(scopeparas, compstr, modifier,  "argument")
          }
        }
        case AbstractVariable("index",_,_) => {
          var lb : ListBuffer[String] = ListBuffer()
          lb += "i0"
          for (i <- 1 to DomainKnowledge.rule_dim() - 1) 
            lb += s"i${i}"
          
          for (i <- 0 to DomainKnowledge.rule_dim() - 1) {
            var loclist : AbstractExpression = new AbstractVariable(lb(i),"",new AbstractLiteral(""))
            args += loclist.transform(scopeparas, compstr, modifier,  "argument")
          }
        }
        case _ => args += a.transform(scopeparas, compstr, modifier, "argument")
      }
      
    }
    
    if (fname.equals("inverse")) {
       if ( DomainKnowledge.stenciltype.equals("nonlinear") && DomainKnowledge.use_gpu) 
         return new ImplValueExpr[String](s"inverse${compstr}(localst[0])")
       else         
      return new ImplBinaryExpr(new ImplValueExpr[String](s"${DomainKnowledge.transform_datatype_cpp(DomainKnowledge.globaldatatype_L2)}(1)"), new OperatorInfo("/"), arglist(0).transform(scopeparas, compstr, modifier, "argument"))
    }

    if (fname.equals("diag")) {

      if (DomainKnowledge.use_gpu) {
        var curStencil = TreeManager.tree.Stencils(0)

        if ( DomainKnowledge.stenciltype.equals("nonlinear") )
          return new ImplValueExpr[String]("localst[0]") // DataClasses.generateStencilConvolutioncuda(1,args(0).toString_cpp,"", "")
        else
          return new ImplValueExpr[String](s"${curStencil.entries(0)(0)}") // DataClasses.generateStencilConvolutioncuda(1,args(0).toString_cpp,"", "")
      } else {
        var expr: ListBuffer[ImplExpression] = ListBuffer(new ImplValueExpr[String]("i0"))
        for (i <- 1 to DomainKnowledge.rule_dim() - 1)
          expr += new ImplValueExpr[String](s"i${i}")

  //      println("was is es? " + args(0) + " " + args(0).toString + " " + args(0).toString_cpp +"\n")
//        return new ImplFcall("Lapl[lev]", fname, expr)
        if ( DomainKnowledge.stenciltype.equals("nonlinear") )  
          return new ImplValueExpr[String]("entries[0]")
        else
          return new ImplFcall(args(0).toString_cpp, fname, expr)
      }
    }
    if (fname.equals("random"))
      return new ImplValueExpr[String](s"(rand()/static_cast<${DomainKnowledge.plaintype}>(RAND_MAX))*" + args(0).toString_cpp) // TODO
      
    if (fname.equals("fasterReduce") && DomainKnowledge.use_gpu) {
      var compstr = ""
      if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) 
        compstr = "0"
          
      if (DomainKnowledge.rule_dim==2) 
        return new ImplValueExpr[String](s"fasterReduce (Res${compstr}[lev].begin(), solution${compstr}[lev].x1_*solution${compstr}[lev].x2_, f${compstr}[lev].begin())") // TODO
      else if (DomainKnowledge.rule_dim==3) 
        return new ImplValueExpr[String](s"fasterReduce (Res${compstr}[lev].begin(), solution${compstr}[lev].x1_*solution${compstr}[lev].x2_*solution${compstr}[lev].x3_, f${compstr}[lev].begin())") // TODO
    }
    
    return new ImplFcall("", fname, args)

  }
}

case class AbstractLiteral(text: String) extends AbstractExpression {
  override def value(context: Context) = text
  override def toString = text 
  override def transform(scopeparas: ListBuffer[ParameterInfo], compstr : String, modifier: Option[String], scopetype: String): ImplExpression = {
    return new ImplValueExpr[String](text)
  }
}

case class AbstractStringLiteral(text: String) extends AbstractExpression {
  override def value(context: Context) = text
  override def toString = text 
  override def transform(scopeparas: ListBuffer[ParameterInfo], compstr : String, modifier: Option[String], scopetype: String): ImplExpression = {
    return new ImplValueExpr[String]("\""+text+"\"")
  }
}

case class AbstractVariable(id: String, comp: String, lev: AbstractExpression) extends AbstractExpression {
  override def value(context: Context) = {
    context.resolve(id) match {
      case Some(binding) => binding.expr.value(context)
      case None => throw new RuntimeException("Unknown identifier: " + id)
    }
  }
  override def toString = id + " " + comp + " " + lev

  override def transform(scopeparas: ListBuffer[ParameterInfo], compstr : String, modifier: Option[String], scopetype: String): ImplExpression = {

    var ti: TypeInfo = new TypeInfo(id, 0)
    var component = ""
    for (e <- TreeManager.tree.Fields)
      if (e.name.equals(id)) {
        ti = new TypeInfo(id, 1)
        if (!compstr.equals(""))
          component = compstr
        else
          component = comp  
        return new ImplVariable("", id, component, ti, lev.transform(scopeparas, compstr, modifier, scopetype), scopetype, modifier.getOrElse(""))
      }

    for (e <- TreeManager.tree.Stencils)
      if (e.name.equals(id)) {
        ti = new TypeInfo(id, 2)
        return new ImplVariable("", id,comp, ti, lev.transform(scopeparas, compstr, modifier, scopetype), scopetype)
        /*  	              if (id.startsWith("Restriction"))
  	                return new VariableInfo("",id , ti, new ValueExpr[String]("0"), scopetype)   	           
  	              else
  	                return new VariableInfo("",id , ti, lev.transform(scopeparas,modifier,scopetype), scopetype) 
			 */ }

    for (e <- scopeparas) {
      //      println(s"inside Var : ${id}  Scope: ${e.name} ${e.dtype}\n")
      if (e.name.equals(id))
        if (e.dtype.startsWith(DomainKnowledge.ArrayClassName)) {
          ti = new TypeInfo(id, 1)
        }
    }

    //  	      	if (id.equals("coarsestlevel"))
    //  	          return new VariableInfo("","lev " + "==" + DomainKnowledge.nlevels_L3.getOrElse(1).toString + "-1", new TypeInfo(id,0), new ImplExpression(), scopetype)

    if (id.contains("Stencil")) {
      ti = new TypeInfo(id, 2)
      return new ImplVariable("", id, comp, ti, new ImplValueExpr[String]("0"), scopetype)
    }

    return new ImplVariable("", id, comp, ti, new ImplExpression(), scopetype, modifier.getOrElse(""))

  }

  override def getvariables: ListBuffer[String] = {
    for (e <- TreeManager.tree.Fields)
      if (e.name.equals(id))
        return ListBuffer(id)
    for (e <- TreeManager.tree.Stencils)
      if (e.name.equals(id))
        return ListBuffer(id)
    return ListBuffer()
  }
}
