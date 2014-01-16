package harald.Abstract

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.Impl._
import harald.ast.TreeManager
import harald.expert.StencilGenerator

sealed abstract class AbstractExpression {
  def value(context: Context): String
  def transform(scopeparas: ListBuffer[ParameterInfo], modifier: Option[String], scopetype: String): ImplExpression
  def getvariables: ListBuffer[String] = ListBuffer()
}

case class AbstractBinaryOp(operator: String, left: AbstractExpression, right: AbstractExpression) extends AbstractExpression {
  override def value(context: Context) = left.value(context)
  override def toString = left.toString + " " + operator + " " + right.toString

  override def transform(scopeparas: ListBuffer[ParameterInfo], modifier: Option[String], scopetype: String): ImplExpression = {
    // check for convolution M * v
    if (operator.equals("*"))
      left match {
        case AbstractVariable(id1, l1) => {
          for (e1 <- TreeManager.tree.Stencils)
            if (e1.name.equals(id1))
              right match {
                case AbstractVariable(id2, l2) => {
                  var lb: ListBuffer[ImplExpression] = new ListBuffer()
                  lb += new ImplVariable("", id2, new TypeInfo(id2, 1), l2.transform(scopeparas, modifier, "argument"), "argument") // TODO

                  if (modifier.getOrElse("").equals("ToCoarse")) {

                    if (!DomainKnowledge.use_gpu) {
                      for (i <- 1 to DomainKnowledge.rule_dim())
                        lb += new ImplValueExpr[String](DomainKnowledge.rule_mapcoarseTofine("i" + (i - 1).toString))

                      //return new functioncall(id1 + s"[0]", "convolve" + e1.length + "P", lb)

                      var memlistS: ListBuffer[ParameterInfo] = ListBuffer()
                      memlistS += new ParameterInfo("fine", TreeManager.tree.ExternalClasses.get("Array").get.name + "<T>&")
                      for (i <- 1 to DomainKnowledge.rule_dim())
                        memlistS += new ParameterInfo(s"2*i${i - 1}", "int")
                      return StencilGenerator.generateStencilConvolution(id1 + "[0]", e1.length, memlistS, "")
                      //  	                     return new functioncall(id1+s"[${l1}]","convolve" + e1.length + "P", lb) 
                    }
                  } else if (modifier.getOrElse("").equals("ToFine")) {

                    for (i <- 1 to DomainKnowledge.rule_dim())
                      lb += new ImplValueExpr[String]("i" + (i - 1).toString)

                    return new ImplFcall(id1 + s"[0]", "interpolate", lb) //ListBuffer(new VariableInfo(id2 , new TypeInfo(id2,1), mapexpression(l2,scopeparas,modifier,"argument").toString, "argument"), new ValueExpr[String]("i0"),new ValueExpr[String]("i1")))
                  } else {

                    if (DomainKnowledge.use_gpu) {

                      var curStencil = TreeManager.tree.Stencils(0)
                      for (st <- TreeManager.tree.Stencils)
                        if (st.name.equals(e1.name))
                          curStencil = st
                      return StencilGenerator.generateStencilConvolutioncuda(e1.length, curStencil, id2, 0, "", "global_idx")

                    } else {
                      for (i <- 1 to DomainKnowledge.rule_dim())
                        lb += new ImplValueExpr[String]("i" + (i - 1).toString)

                      // return new functioncall(id1 + s"[0]", "convolve" + e1.length + "P", lb)
                      //  	                         return new functioncall(id1+s"[${l1}]","convolve" + e1.length + "P", lb) 

                      var memlistS: ListBuffer[ParameterInfo] = ListBuffer()
                      memlistS += new ParameterInfo("solution[lev]", TreeManager.tree.ExternalClasses.get("Array").get.name + "<T>&") // TODO: this really shouldn't be hardcoded
                      for (i <- 1 to DomainKnowledge.rule_dim())
                        memlistS += new ParameterInfo(s"i${i - 1}", "int")
                      return StencilGenerator.generateStencilConvolution(id1 + "[0]", e1.length, memlistS, "")

                    }
                  }
                }
                case _ =>
              }
        }
        case _ =>
      }

    return new ImplBinaryExpr(left.transform(scopeparas, modifier, scopetype), new OperatorInfo(operator), right.transform(scopeparas, modifier, scopetype))
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

  override def transform(scopeparas: ListBuffer[ParameterInfo], modifier: Option[String], scopetype: String): ImplExpression = {
    var args: ListBuffer[ImplExpression] = ListBuffer()
    for (a <- arglist)
      args += a.transform(scopeparas, modifier, "argument")

    if (fname.equals("inverse")) {
      return new ImplBinaryExpr(new ImplValueExpr[String](s"${DomainKnowledge.datatype_L2.getOrElse("double")}(1)"), new OperatorInfo("/"), arglist(0).transform(scopeparas, modifier, "argument"))
    }

    if (fname.equals("diag")) {

      if (DomainKnowledge.use_gpu) {
        var curStencil = TreeManager.tree.Stencils(0)

        return new ImplValueExpr[String](s"${curStencil.entries(0)}") // DataClasses.generateStencilConvolutioncuda(1,args(0).toString_cpp,"", "")
      } else {
        var expr: ListBuffer[ImplExpression] = ListBuffer(new ImplValueExpr[String]("i0"))
        for (i <- 1 to DomainKnowledge.rule_dim() - 1)
          expr += new ImplValueExpr[String](s"i${i}")

        return new ImplFcall(args(0).toString_cpp, fname, expr)
      }
    }
    if (fname.equals("random"))
      return new ImplValueExpr[String]("(rand()/static_cast<double>(RAND_MAX))*" + args(0).toString_cpp) // TODO
    if (fname.equals("fasterReduce") && DomainKnowledge.use_gpu)
      return new ImplValueExpr[String]("fasterReduce (Res[lev].begin(), solution[lev].x1_*solution[lev].x2_, f[lev].begin())") // TODO

    return new ImplFcall("", fname, args)

  }
}

case class AbstractLiteral(text: String) extends AbstractExpression {
  override def value(context: Context) = text
  override def toString = text 
  override def transform(scopeparas: ListBuffer[ParameterInfo], modifier: Option[String], scopetype: String): ImplExpression = {
    return new ImplValueExpr[String](text)
  }
}

case class AbstractStringLiteral(text: String) extends AbstractExpression {
  override def value(context: Context) = text
  override def toString = text 
  override def transform(scopeparas: ListBuffer[ParameterInfo], modifier: Option[String], scopetype: String): ImplExpression = {
    return new ImplValueExpr[String]("\""+text+"\"")
  }
}

case class AbstractVariable(id: String, lev: AbstractExpression) extends AbstractExpression {
  override def value(context: Context) = {
    context.resolve(id) match {
      case Some(binding) => binding.expr.value(context)
      case None => throw new RuntimeException("Unknown identifier: " + id)
    }
  }
  override def toString = id + " " + lev

  override def transform(scopeparas: ListBuffer[ParameterInfo], modifier: Option[String], scopetype: String): ImplExpression = {

    var ti: TypeInfo = new TypeInfo(id, 0)
    for (e <- TreeManager.tree.Fields)
      if (e.name.equals(id)) {
        ti = new TypeInfo(id, 1)
        return new ImplVariable("", id, ti, lev.transform(scopeparas, modifier, scopetype), scopetype)
      }

    for (e <- TreeManager.tree.Stencils)
      if (e.name.equals(id)) {
        ti = new TypeInfo(id, 2)
        return new ImplVariable("", id, ti, new ImplValueExpr[String]("0"), scopetype)
        /*  	              if (id.startsWith("Restriction"))
  	                return new VariableInfo("",id , ti, new ValueExpr[String]("0"), scopetype)   	           
  	              else
  	                return new VariableInfo("",id , ti, lev.transform(scopeparas,modifier,scopetype), scopetype) 
			 */ }

    for (e <- scopeparas) {
      //      println(s"inside Var : ${id}  Scope: ${e.name} ${e.dtype}\n")
      if (e.name.equals(id))
        if (e.dtype.startsWith(TreeManager.tree.ExternalClasses.get("Array").get.name)) {
          ti = new TypeInfo(id, 1)
        }
    }

    //  	      	if (id.equals("coarsestlevel"))
    //  	          return new VariableInfo("","lev " + "==" + DomainKnowledge.nlevels_L3.getOrElse(1).toString + "-1", new TypeInfo(id,0), new ImplExpression(), scopetype)

    if (id.contains("Stencil")) {
      ti = new TypeInfo(id, 2)
      return new ImplVariable("", id, ti, new ImplValueExpr[String]("0"), scopetype)
    }

    return new ImplVariable("", id, ti, new ImplExpression(), scopetype)

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
