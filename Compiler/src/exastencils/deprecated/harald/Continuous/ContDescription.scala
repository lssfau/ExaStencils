package exastencils.deprecated.harald.Continuous

case class ContInterval(left : Int, right : Int, inttype : String) {

  var values = List(left, right)

  override def toString = s"[${ left },${ right }] ${ inttype }"
  def ToStringClass = "ContInterval"
}

case class ContSubDomain(intervals : List[ContInterval]) {
  override def toString = {
    var str = ""
    str = intervals(0).toString
    for (i <- 1 to intervals.size - 1)
      str += " x " + intervals(i).toString
    str
  }
  def ToStringClass = "ContSubDomain"
}

case class ContDomain(name : String, subdoms : List[ContSubDomain]) {
  override def toString = {
    var str = s"${ name } "
    str += subdoms(0).toString
    for (i <- 1 to subdoms.size - 1)
      str += " and " + subdoms(i).toString
    str
  }
  def ToStringClass = "ContDomain"
}

object ContDescription {
/*
  def setup {
    
    var contdt = "R"
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) 
      contdt = "C"
       
    val domainomega = new ContFunctionType(DomainKnowledge.domain_L1.get._2, DomainKnowledge.rule_dim, 1, contdt, 1, 1)
    val domainomegat = new ContFunctionType(DomainKnowledge.domain_L1.get._2, DomainKnowledge.rule_dim+1, 1, contdt, 1, 1)
    val domainomega2 = new ContFunctionType(DomainKnowledge.domain_L1.get._2, DomainKnowledge.rule_dim, DomainKnowledge.rule_dim, contdt, 1, DomainKnowledge.rule_dim)
    val optype1 = new ContOperatorType(domainomega,domainomega)
    val optype1t = new ContOperatorType(domainomegat,domainomega)
    val optype2 = new ContOperatorType(domainomega,domainomega2)
    val optype2t = new ContOperatorType(domainomegat,domainomega2)
    val optype2tt = new ContOperatorType(domainomegat,domainomega)
    val optype3 = new ContOperatorType(domainomega2,domainomega)
    val optype4 = new ContOperatorType(domainomega2,domainomega2)
    
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) {
      
    val funcf = new ContFunction("f", domainomega)
    DomainKnowledge.cont_functions += funcf
    val funcu = new ContFunction("u", domainomega)
    DomainKnowledge.cont_functions += funcu
    val funcg = new ContFunction("g", domainomega)
    DomainKnowledge.cont_functions += funcg
    val naop = new ContOperator("nabla", optype2)
    DomainKnowledge.cont_operators += naop
    val divop = new ContOperator("div", optype3)
    DomainKnowledge.cont_operators += divop
    
    DomainKnowledge.cont_equations += new ContEquation(new ContIntegral("d"+DomainKnowledge.domain_L1.get._2, new ContBinOp("*", new ContBinOp("*", new ContVariable(funcg), new ContOpapply(naop,funcu)),new ContTranspose(new ContLiteral("n")) )), new ContIntegral(DomainKnowledge.domain_L1.get._2, new ContVariable(funcf)), DomainKnowledge.domain_L1.get._2)
    DomainKnowledge.cont_equations += new ContEquation(new ContIntegral("d"+DomainKnowledge.domain_L1.get._2, new ContBinOp("*", new ContVariable(funcg), new ContOpapply(naop,funcu))), new ContLiteral("0"), "d"+DomainKnowledge.domain_L1.get._2)
    
    } else if (DomainKnowledge.operator_L1(0)._2(0).equals("Laplacian")) {
      
    val funcf = new ContFunction("f", domainomega)
    DomainKnowledge.cont_functions += funcf
    val funcu = new ContFunction("u", domainomega)
    DomainKnowledge.cont_functions += funcu
    val laop = new ContOperator("Laplace", optype1)
    DomainKnowledge.cont_operators += laop
    
    DomainKnowledge.cont_equations += new ContEquation(new ContUnaryOp("-", new ContOpapply(laop,funcu)),  new ContVariable(funcf), DomainKnowledge.domain_L1.get._2)
    DomainKnowledge.cont_equations += new ContEquation(new ContVariable(funcu), new ContLiteral("0"), "d"+DomainKnowledge.domain_L1.get._2)

    } else if (DomainKnowledge.operator_L1(0)._2(0).equals("OpticalFlow")) {
  
    val funcu = new ContFunction("u", domainomega2)
    DomainKnowledge.cont_functions += funcu
    val laop = new ContOperator("Laplace", optype4)
    DomainKnowledge.cont_operators += laop
    val funcI = new ContFunction("I", domainomegat)
    DomainKnowledge.cont_functions += funcI
    
    val naop = new ContOperator("nabla_x", optype2t)
    DomainKnowledge.cont_operators += naop
    val naopt = new ContOperator("nabla_t", optype2tt)
    DomainKnowledge.cont_operators += naopt
    val alpha = new ContConstant("alpha",new ContConstType(contdt, 1))
    
    } else if (DomainKnowledge.operator_L1(0)._2(0).equals("Stokes")) {
      
    val funcf = new ContFunction("f", domainomega2)
    DomainKnowledge.cont_functions += funcf
    val funcp = new ContFunction("p", domainomega)
    DomainKnowledge.cont_functions += funcp
    val funcu = new ContFunction("u", domainomega2)
    DomainKnowledge.cont_functions += funcu
    val laop = new ContOperator("Laplace", optype4)
    DomainKnowledge.cont_operators += laop
    val naop = new ContOperator("nabla", optype2)
    DomainKnowledge.cont_operators += naop
    val divop = new ContOperator("div", optype3)
    DomainKnowledge.cont_operators += divop
    
    DomainKnowledge.cont_equations += new ContEquation(new ContIntegral("Omega",new ContVariable(funcp)), new ContLiteral("0"), "Omega")
    DomainKnowledge.cont_equations += new ContEquation(new ContOpapply(divop,funcu), new ContLiteral("0"), "Omega")
    DomainKnowledge.cont_equations += new ContEquation(new ContBinOp("+",new ContUnaryOp("-", new ContOpapply(laop,funcu)), new ContOpapply(naop,funcp)), new ContVariable(funcf), "Omega")
    DomainKnowledge.cont_equations += new ContEquation(new ContVariable(funcu), new ContVecLiteral(List("0","0")), "dOmega")
    DomainKnowledge.cont_equations += new ContEquation(new ContVariable(funcf), new ContVecExpression(List(new ContLiteral("x"),new ContFuncapply("sin",new ContLiteral("y")))), "dOmega")
    
    funcu.funcexpr += DomainKnowledge.cont_equations(3)
    funcf.funcexpr += DomainKnowledge.cont_equations(4)
    
    }

    
    
    for (i <- DomainKnowledge.cont_functions)
    println(s"${i}")
    for (i <- DomainKnowledge.cont_operators)
    println(s"${i}")
    for (i <- DomainKnowledge.cont_equations)
    println(s"${i}")
  }
  */
}