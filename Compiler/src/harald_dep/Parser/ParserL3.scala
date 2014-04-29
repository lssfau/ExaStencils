package harald_dep.Parser

import harald_dep.dsl.DomainKnowledge

class ParserL3 extends ExaParser
{

    class ExaOption(val name: String, val value: Expr) {
	}
    class ExaStringOption(val name: String, val value: String) {
	}
	
	def exastencilsL3: Parser[Any] =  multigridcomponents.? <~ newline.? ~> multigridparameter.? 	
	
	def multigridparameter: Parser[Any] = "mgparameter" ~ "{" ~> {option.* ^^ { case opts => for(o <- opts) { set(DomainKnowledge, o.name, evaluate(o.value)) }; opts } } <~ "}" 
	def multigridcomponents: Parser[Any] = "mgcomponents" ~ "{" ~> {stringoption.* ^^ { case opts => for(o <- opts) { set(DomainKnowledge, o.name, o.value) }; opts } } <~ "}" 
	
	def option: Parser[ExaOption] = { ident ~ "=" ~ expr <~ newline.? } ^^ {case k ~ eq ~ v => ; new ExaOption(k, v) }
	def stringoption: Parser[ExaStringOption] = { ident ~ "=" ~ (smoother_type | interpolation_type | restriction_type | coarse_type) <~ newline.? } ^^ {case k ~ eq ~ v => ; new ExaStringOption(k, v) } 
	
	
	def field_datatype: Parser[String] = "real" | "natural"	
	def field_location: Parser[String] = "nodes" | "cells"
	
	def smoother_type: Parser[String] = ident | "GS" | "Jac"
	def interpolation_type: Parser[String] = ident | "Linear"
	def restriction_type: Parser[String] = ident | "FW"
	def coarse_type: Parser[String] = ident | "CG" | "Direct" | "AMG"
	
}
