package harald.Parser

import harald.dsl.DomainKnowledge

class ParserHW extends ExaParser
{
    class ExaOption(val name: String, val value: Expr) {
	}
    	
	def exastencilsHW: Parser[Any] = hardware.* <~ newline.? ~> node.? <~ newline.? ~> cluster.? 
	
	def hardware: Parser[Any] = "Hardware" ~ {ident ^^ { case v => set(DomainKnowledge, "hardware_HW", v); v}} ~ "{" ~> {option.* ^^ { case opts => for(o <- opts) { set(DomainKnowledge, o.name, evaluate(o.value)) }; opts } } <~ "}" 
	def node: Parser[Any] = "Node" ~ {ident ^^ { case v => set(DomainKnowledge, "node_HW", v); v}} ~ "{" ~> {option.* ^^ { case opts => for(o <- opts) { set(DomainKnowledge, o.name, evaluate(o.value)) }; opts } } <~ "}" 
	def cluster: Parser[Any] = "Cluster" ~ {ident ^^ { case v => set(DomainKnowledge, "cluster_HW", v); v}} ~ "{" ~> {option.* ^^ { case opts => for(o <- opts) { set(DomainKnowledge, o.name, evaluate(o.value)) }; opts } } <~ "}" 

	def option: Parser[ExaOption] = { ident ~ "=" ~ expr <~ newline.? } ^^ {case k ~ eq ~ v => ; new ExaOption(k, v) }
	
}
