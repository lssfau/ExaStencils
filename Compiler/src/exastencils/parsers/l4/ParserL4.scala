package exastencils.parsers.l4

import exastencils.parsers._
import exastencils.datastructures._
import exastencils.datastructures.l4._

class ParserL4 extends ExaParser /*with scala.util.parsing.combinator.syntactical.TokenParsers*/ with scala.util.parsing.combinator.PackratParsers {
  def parse1(input : String) {
    scanner = new lexical.Scanner(input)
    val result = phrase(program)(scanner)
    println(result.toString)
    println(".................")
    println(".................")
    
    result.get.foreach(f => println(f.getAnnotations.foreach(a => println(a))))//{ if(f.isInstanceOf[Annotatable]) println(f.asInstanceOf[Annotatable].getAnnotations) })
    //    result match {
    //      case Success(tree, _) => new Interpreter(tree).run()
    //
    //      case e: NoSuccess => {
    //        println(e)
    //      }
    //    }
    //
    //    println(result.toString)
    //    for (e <- tree.exaClasses)
    //      println(e)
    
    println("source"+scanner.source)
  }
  
  
   def parseTokens(tokens: lexical.Scanner): Node = {
      phrase(program)(tokens) match {
        case Success(e, _) => e.head // FIXME
        case Error(msg, _) => throw new Exception("parse error: " + msg)
        case Failure(msg, _) => throw new Exception("parse failure: " + msg)
      }
    }

    /*** External Interface ***/

    def parse2(s: String): Node = {
      parseTokens(new lexical.Scanner(s))
    }

    def parseFile(filename: String): Node = {
      //val reader = scala.util.parsing.input.StreamReader(new java.io.InputStreamReader(new java.io.FileInputStream(filename)))
      
      import scala.util.parsing.input._
      import scala.collection.immutable.PagedSeq
      
      val lines = io.Source.fromFile(filename).getLines
      reader = new PagedSeqReader(PagedSeq.fromLines(lines))
      scanner = new lexical.Scanner(reader)
      
    
      val x = parseTokens(scanner)
      x
    }

    

  lazy val program = function.*

  lazy val statement : Parser[Statement] = locationize(function) |
    loopOverDomain |
    locationize(substatement)

  lazy val substatement : Parser[Statement] = locationize(ident ~ "=" ~ expression ^^ { case id ~ "=" ~ exp => AssignmentStatement(Identifier(id), exp) })

  lazy val function = locationize(  ("def" ~> ident) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> datatype) ~ ("{" ~> (statement.* <~ "}")) ^^
    { case id ~ args ~ t ~ stmts => FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts) })
  
//  lazy val function = new Parser[this.lexical.Token] {
//    def apply(in : Input) : ParseResult[lexical.Token] ={
//    		val c = in.first
//    		if(c == "def") Success(c, in.rest) else Failure("not def found", in)
//    }
//    	
//}
//  import util.parsing.combinator._
//  object FunctionParser extends ExaParser {
//    
//  //lazy val accept: Parser[List[AcceptHeader]] = rep1sep(acceptEntry, ",")
//    
//    lazy val function = ("def" ~> ident) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> datatype) ~ ("{" ~> (statement.* <~ "}")) ^^
//    { case id ~ args ~ t ~ stmts => Make(FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts)) }
//      lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => arg :: args }
//  lazy val functionArgument = ((ident <~ ":") ~ datatype) ^^ { case id ~ t => Make(Variable(id, t)) }
//
//  def parse(input: String): FunctionStatement = parse(function, input)
//}
    
//    import scala.util.parsing.input._
//   override def positioned[T <: Positional](p: => Parser[T]): Parser[T] = Parser { println("hallo")
//    in =>
//    p(in) match {
//      case Success(t, in1) => Success(if (t.pos == NoPosition) t setPos in.pos else t, in1)
//      case ns: NoSuccess => ns
//    }
//  }
  
  
  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => arg :: args }
  lazy val functionArgument = ((ident <~ ":") ~ datatype) ^^ { case id ~ t => Variable(id, t) }
  lazy val functionCall = ident ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCall(id, args.getOrElse(List[Expression]())) }
  lazy val functionCallArgumentList = (expression <~ ("," | newline)).* ~ expression ^^ { case exps ~ ex => ex :: exps } // = new list(exps, ex)

  lazy val expression : PackratParser[Expression] = 
    (expression ~ ("+" | "-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) } |
      term

  lazy val term : PackratParser[Expression] =
    (term ~ ("*" | "/" | "**") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) } |
      factor

  lazy val factor : Parser[Expression] =
    "(" ~> expression <~ ")" |
      stringLit ^^ { case s => StringLiteral(s) } |
      ("+" | "-").? ~ numericLit ^^ { case s ~ n => if (s == Some("-")) NumericLiteral(-n.toDouble) else NumericLiteral(n.toDouble) } | // FIXME check and create integer/double
      booleanLit ^^ { case s => BooleanLiteral(s.toBoolean) } |
      locationize(functionCall) |
      locationize(ident ^^ { case id => Identifier(id) })

  lazy val loopOverDomain = ("loop" ~ "over" ~> ("domain" | "inner" | "boundary")) ~ ("level" ~> ident) ~
    ("order" ~> ident).? ~ ("blocksize" ~> (numericLit ~ numericLit ~ numericLit)).? ~
    substatement.+ <~ "next" ^^
    { case area ~ level ~ order ~ blocksize ~ stmts => LoopOverDomainStatement(area, level, order, blocksize, stmts) }
}
