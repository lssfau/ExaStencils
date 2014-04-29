package harald.Parser

import harald.dsl._
import harald.Abstract._
import harald.ast._

class ParserL2(tree : TreeL2) extends ExaParser
{

    class ExaOption(val name: String, val value: Expr) {
	}
    	
	def exastencilsL2: Parser[Any] = fragments.? <~ newline.? ~> discdomain.? <~ newline.? ~> field.* <~ newline.? ~> stencil.* 
	
	def fragments: Parser[Any] = ("Fragments" ~> ident) ~ ("=" ~> fragment_type) ^^ { case n~t => set(DomainKnowledge, "fragment_L2", Tuple2(n,t))}
	def discdomain: Parser[Any] = "Discrete_Domain" ~ {ident ^^ { case v => set(DomainKnowledge, "discrete_domain_L2", v); v}} ~ "levels" ~ {wholeNumber ^^ { case v => DomainKnowledge.nlevels_L2=Some(v.toInt); v}} ~ "{" ~> {option.* ^^ { case opts => for(o <- opts) { set(DomainKnowledge, o.name, evaluate(o.value)) }; opts } } <~ "}" 

    def fragment_type: Parser[String] = "Regular_Square" | "Regular_Cube"
    
	def field: Parser[Any] = "DiscreteFunction" ~ "<" ~ field_datatype ~ "," ~ wholeNumber ~ ">" ~ "@" ~ field_location ~ ident ^^ { 
//	  case a~b~dt~c~fl~d~e~loc~id => tree.exaFields.append(new AbstractField(id, dt, fl, loc)) 
	  case a~b~dt~c~fl~d~e~loc~id => 
	    for (f <- DomainKnowledge.cont_functions)
	      if (f.name.equals(id))
	        DomainKnowledge.discr_functions += f.discretize(dt,loc) 
	  }
	
//	def stencil: Parser[Any] = "stencil" ~ "<" ~ field_datatype ~ "," ~ discretization_type ~ ">" ~ ident ^^ { case a~b~dt~c~distype~d~id => ExaDSL.exaStencils.append(new ExaDSL.ExaStencil(id, dt, distype)) }
	def stencil: Parser[Any] = "DiscreteOperator" ~ "<" ~ field_datatype ~ "," ~ wholeNumber ~ "," ~ wholeNumber ~ "," ~ discretization_type ~ "," ~ wholeNumber ~ ">" ~ "@" ~ field_location ~ ident ^^ { 
	  case a~b~dt~d~ml1~e~ml2~f~distype~g~ac~h~i~loc~id => 
//	    tree.exaOperators.append(new AbstractStencil(id, dt, ml1, ml2, distype, ac, loc)) 
	    for (o <- DomainKnowledge.cont_operators)
	      if (o.name.equals(id))
	        DomainKnowledge.discr_operators += o.discretize(dt,loc) 
	    }
	
	def field_datatype: Parser[String] = "Float" | "Int" | "Double" | "ComplexDouble" | "ComplexFloat" 	
	def field_location: Parser[String] = "nodes" | "cells"
	
	def discretization_type: Parser[String] = "FD" | "FE"
	def option: Parser[ExaOption] = { ident ~ "=" ~ expr <~ newline.? } ^^ {case k ~ eq ~ v => ; new ExaOption(k, v) }
	
}
