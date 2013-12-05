package exastencils.parsers.l4

import exastencils.parsers._
import exastencils.datastructures._
import exastencils.datastructures.l4._

class ParserL4 extends ExaParser with scala.util.parsing.combinator.PackratParsers {
  def parse(s : String) : Node = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : Node = {
    import scala.util.parsing.input._
    import scala.collection.immutable.PagedSeq

    val lines = io.Source.fromFile(filename).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    parseTokens(scanner)
  }

  protected def parseTokens(tokens : lexical.Scanner) : Node = {
    phrase(program)(tokens) match {
      case Success(e, _)   => e // FIXME
      case Error(msg, _)   => throw new Exception("parse error: " + msg)
      case Failure(msg, _) => throw new Exception("parse failure: " + msg)
    }
  }

  lazy val program = locationize(function.* ^^ { case stmts => Root(stmts) })

  lazy val statement : Parser[Statement] = function |
    loopOverDomain |
    substatement

  lazy val substatement : Parser[Statement] = locationize(ident ~ "=" ~ expression ^^ { case id ~ "=" ~ exp => AssignmentStatement(Identifier(id), exp) })

  lazy val function = locationize(("def" ~> ident) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> datatype) ~ ("{" ~> (statement.* <~ "}")) ^^
    { case id ~ args ~ t ~ stmts => FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts) })

  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => arg :: args }
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => Variable(id, t) })
  lazy val functionCall = locationize(ident ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCall(id, args.getOrElse(List[Expression]())) })
  lazy val functionCallArgumentList = (expression <~ ("," | newline)).* ~ expression ^^ { case exps ~ ex => ex :: exps } // = new list(exps, ex)

  lazy val expression : PackratParser[Expression] =
    locationize((expression ~ ("+" | "-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) }) |
      term

  lazy val term : PackratParser[Expression] =
    locationize((term ~ ("*" | "/" | "**") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) }) |
      factor

  lazy val factor : Parser[Expression] =
    "(" ~> expression <~ ")" |
      locationize(stringLit ^^ { case s => StringLiteral(s) }) |
      locationize(("+" | "-").? ~ numericLit ^^ { case s ~ n => if (s == Some("-")) NumericLiteral(-n.toDouble) else NumericLiteral(n.toDouble) }) | // FIXME check and create integer/double
      locationize(booleanLit ^^ { case s => BooleanLiteral(s.toBoolean) }) |
      locationize(functionCall) |
      locationize(ident ^^ { case id => Identifier(id) })

  lazy val loopOverDomain = locationize(("loop" ~ "over" ~> ("domain" | "inner" | "boundary")) ~ ("level" ~> ident) ~
    ("order" ~> ident).? ~ ("blocksize" ~> (numericLit ~ numericLit ~ numericLit)).? ~
    substatement.+ <~ "next" ^^
    { case area ~ level ~ order ~ blocksize ~ stmts => LoopOverDomainStatement(area, level, order, blocksize, stmts) })
}
