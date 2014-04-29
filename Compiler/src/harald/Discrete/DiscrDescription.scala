package harald.Discrete

import harald.dsl._
import scala.collection.mutable.ListBuffer

object DiscrDescription {

  def setup(datatype : String, location : String) {

  /*  for (i <- DomainKnowledge.cont_functions)
      DomainKnowledge.discr_functions += i.discretize(datatype,location)
    for (i <- DomainKnowledge.cont_operators)
     DomainKnowledge.discr_operators += i.discretize(datatype,location)     
  */  for (i <- DomainKnowledge.cont_consts)
      DomainKnowledge.discr_consts += i.discretize(datatype,location)
      
  }
}

case class DiscrPoint(p: List[Int]) {
    override def toString = p.toString
}

case class DiscrSet(name : String, subsets: List[Tuple2[DiscrPoint,DiscrPoint]]) {
    
  override def toString = name + s" [${subsets.toString}]"
}

case class DiscrIteration(val name : String, val eq : DiscrExpression, val order : String, val sequence : List[Tuple2[DiscrExpression,DiscrExpression]] ) {
  override def toString = name + ":" + eq + " order " + order + " sequence " + sequence 
 
  def printtoDSL4 : String = {
    var paramlist : ListBuffer[Tuple2[String,String]] = ListBuffer()
    if (!eq.ToStringClass.equals("DiscrExpression")) {
      
    for (s <- eq.getvar.distinct) {
//      println(s)
      for (f1 <- DomainKnowledge.discr_functions)
        if (s.startsWith(f1.name))
          paramlist += Tuple2(s,"Array")
      for (f2 <- DomainKnowledge.discr_consts)
        if (f2.name.equals(s))
          paramlist += Tuple2(s,f2.consttype.domain)
      for (f3 <- DomainKnowledge.discr_operators)
        if (s.startsWith(f3.name))
          paramlist += Tuple2(s,"Stencil")
    }
      
    } else {
      
    for (seq <- sequence)
    for (s <- seq._2.getvar.distinct) {
      for (f1 <- DomainKnowledge.discr_functions)
       if (s.startsWith(f1.name))
           paramlist += Tuple2(s,"Array")
      for (f2 <- DomainKnowledge.discr_consts)
        if (f2.name.equals(s))
          paramlist += Tuple2(s,f2.consttype.domain)
      for (f3 <- DomainKnowledge.discr_operators)
        if (s.startsWith(f3.name))
          paramlist += Tuple2(s,"Stencil")
    }
    
    }
    var paramliststr = ""
    for (s <- paramlist)
      paramliststr += " " + s._1 + " : " + s._2
      
    var str = ""
    for (l <- 0 to DomainKnowledge.nlevels_L2.getOrElse(1)-1) {  
     str += s"def cpu ${name}_${l} ( ) :  Unit \n"   // ${paramliststr} 
     str +=  "{ \n"
       
     if (!order.equals("")) {
      for (o <- DomainKnowledge.discr_sets)
        if (o.name.equals(order))
          str += s"loop innerpoints order ${order} \n"
     }    
     
     if (!eq.ToStringClass.equals("DiscrExpression")) {
       str += eq.printtoDSL4(l) + "\n"
  /*     for (s <- eq.getvar.distinct)
         for (f1 <- DomainKnowledge.discr_iterations)
           if (f1.name.equals(s))
             str += f1.eq.getvar.distinct + "\n"
*/
     }  else {
       for (seq <- sequence) {
       seq._1 match {
         case DiscrBinOp("==", DiscrLiteral(lhs,""), DiscrLiteral(rhs,"")) => if (l == rhs.toInt) str += seq._2.printtoDSL4(l)   
         case DiscrBinOp("!=", DiscrLiteral(lhs,""), DiscrLiteral(rhs,"")) => if (l != rhs.toInt) str += seq._2.printtoDSL4(l)
       }
 //      str += "if (" + seq._1.printtoDSL4 + " ) {"
 //      str += seq._2.printtoDSL4 + "} \n"
 /*      for (s <- seq._2.getvar.distinct)
         for (f1 <- DomainKnowledge.discr_iterations)
           if (f1.name.equals(s))
             str += s + "\n"
   */    }
     }  
     if (!order.equals("")) {
      for (o <- DomainKnowledge.discr_sets)
        if (o.name.equals(order))
          str += s"next \n"
     }    
     str += "}  \n"
    }
     return str    
  }
}
