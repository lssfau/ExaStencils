package exastencils.deprecated.harald.Parser

import exastencils.deprecated.harald.dsl.DomainKnowledge
import exastencils.deprecated.harald.dsl.ExaKnowledge

class ParserHW extends ExaParser
{
    class ExaOption(val name: String, val value: Expr) {
	}
    	
	def exastencilsHW: Parser[Any] = hardware.* <~ newline.? ~> node.? <~ newline.? ~> cluster.? 
	
	def hardware: Parser[Any] = "Hardware" ~ {ident ^^ { case v => DomainKnowledge.hardware_HW = Some(v); v}} ~ "{" ~> {option.* ^^ { case opts => for(o <- opts) { set(DomainKnowledge, o.name, evaluate(o.value)) }; opts } } <~ "}" 
	def node: Parser[Any] = "Node" ~ {ident ^^ { case v => DomainKnowledge.node_HW = Some(v); v}} ~ "{" ~> {option.* ^^ { case opts => for(o <- opts) { set(DomainKnowledge, o.name, evaluate(o.value)) }; opts } } <~ "}" 
	def cluster: Parser[Any] = "Cluster" ~ {ident ^^ { case v => DomainKnowledge.cluster_HW = Some(v); v}} ~ "{" ~> {option.* ^^ { case opts => for(o <- opts) { set(DomainKnowledge, o.name, evaluate(o.value)) }; opts } } <~ "}" 

	def option: Parser[ExaOption] = { ident ~ "=" ~ expr <~ newline.? } ^^ {case k ~ eq ~ v => ; new ExaOption(k, v) }
	
}
