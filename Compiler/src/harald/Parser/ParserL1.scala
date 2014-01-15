package harald.Parser

import harald.dsl.DomainKnowledge

class ParserL1 extends ExaParser
{

	// DSL level 1 parser
	def exastencilsL1: Parser[Any] = domain.? <~ newline.? ~> function.* <~ newline.? ~> unknown.* <~ newline.? ~> operator.* <~ newline.? ~> pde.? <~ newline.? ~> pdebc.? <~ newline.? ~> accuracy.? <~ newline.? ~> generate.?

	def domain: Parser[Any] = ("Domain" ~> ident) ~ ("=" ~> domain_type) ^^ { case n~t => set(DomainKnowledge, "domain_L1", Tuple2(n,t))}
	def function: Parser[Any] = ("Function" ~> ident) ~ ("=" ~> expr)^^ { case fname~fexpr => DomainKnowledge.function_L1 += new Tuple2(fname,evaluate(fexpr))}
	def unknown: Parser[Any] = ("Unknown" ~> ident) ~ ("=" ~> expr) ^^ { case n~fexpr => DomainKnowledge.unknown_L1  += new Tuple2(n, evaluate(fexpr))}
	def operator: Parser[Any] = ("Operator" ~> ident) ~ ("=" ~> ident)^^ { case n~op => DomainKnowledge.operator_L1 +=  new Tuple2(n,op)}

	def pde: Parser[Any] = ("PDE" ~> ident) ~ ("{" ~> ident) ~ ("(" ~> ident) ~ (")" ~ "=" ~> ident <~ "}") ^^ { case n1~n2~n3~n4 => set(DomainKnowledge, "pde_L1", n1 + " " + n2 + " " + n3 + " " + n4)}
	def pdebc: Parser[Any] = ("PDEBC" ~> ident) ~ ("{" ~> ident) ~ ("=" ~> ident <~ "}") ^^ { case n~rhs~lhs => set(DomainKnowledge, "pdebc_L1", Tuple2(rhs,lhs))}
	def accuracy: Parser[Any] = "Accuracy" ~ "=" ~ expr ^^ { case a~b~e => set(DomainKnowledge, "accuracy_L1", evaluate(e))}
	def generate: Parser[Any] = "Generate" ~ "=" ~ expr ^^ { case a~b~e => set(DomainKnowledge, "generate_L1", evaluate(e))}
	
    def domain_type: Parser[String] = "UnitSquare" | "Square" | "UnitCube"
//	def operator_type: Parser[String] = "Laplacian"
			
}
