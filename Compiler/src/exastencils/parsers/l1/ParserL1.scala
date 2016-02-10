package exastencils.parsers.l1

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader

import exastencils.datastructures.Node
import exastencils.datastructures.l1._
import exastencils.parsers.ExaParser

class ParserL1 extends ExaParser {
  override val lexical = new LexerL1()

  def parseFile(filename : String) : Node = {
    val lines = io.Source.fromFile(filename).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)
    parseTokens(scanner)
  }

  protected def parseTokens(tokens : lexical.Scanner) : Node = {
    phrase(program)(tokens) match {
      case Success(e, _) => e
      case Error(msg, _) => throw new Exception("parse error: " + msg)
      case Failure(msg, parser) => {
        var sb = new StringBuilder
        sb.append(s"Parse failure at position ${parser.pos}: $msg\n")
        sb.append(parser.pos.longString)
        sb.append("\n")
        throw new Exception(sb.toString)
      }
    }
  }

  // ######################################
  // ##### basic definitions
  // ######################################

  lazy val program = (domain ||| operator ||| equation ||| mapping).+ ^^ { case x => Root(x) }

  lazy val domain = ("Domain" ~> ident) ~ ("=" ~> range) ^^ { case id ~ range => Domain(id, range) }
  lazy val operator = ("Operator" ~> ident) ~ ("=" ~> binaryexpression) ^^ { case id ~ exp => Operator(id, exp) }
  lazy val equation = ("Equation" ~> binaryexpression) ~ ("=" ~> binaryexpression) ^^ { case l ~ r => Equation(l, r) }
  lazy val mapping = ("Mapping" ~> ident) ~ (arrow ~> set) ^^ { case id ~ set => Mapping(id, set) }

  // ######################################
  // ##### range definitions
  // ######################################

  lazy val range = range1d ||| range2d ||| range3d

  lazy val range1d = ("[" ~> realLit <~ ",") ~ (realLit <~ "]") ^^ { case a ~ b => List(Tuple2(a, b)) }
  lazy val range2d = (range1d <~ rangeMultiply) ~ range1d ^^ { case a ~ b => a ::: b }
  lazy val range3d = (range2d <~ rangeMultiply) ~ range1d ^^ { case a ~ b => a ::: b }
  lazy val rangeMultiply = "\\times" ||| "\u00D7" ||| "*"

  // ######################################
  // ##### mapping definitions
  // ######################################

  lazy val arrow = "-" ~ ">"
  lazy val set = (
    ("C" ||| "R") ~ ("^" ~> ("1" ||| "2" ||| "3")) ^^ { case id ~ exp => MathSet(id, exp) }
    ||| ident ^^ { case id => MathSet(id) })

  // ######################################
  // ##### binary expressions
  // ######################################

  lazy val binaryexpression : PackratParser[Expression] = (
    ((binaryexpression ~ ("+" ||| "-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| term)

  lazy val term : PackratParser[Expression] = (
    ((term ~ ("*" ||| "/") ~ term2) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| term2)

  lazy val term2 : PackratParser[Expression] = (
    ((term2 ~ ("**" ||| "^") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| factor)

  lazy val factor : PackratParser[Expression] = (
    "(" ~> binaryexpression <~ ")"
    ||| "{" ~> binaryexpression <~ "}"
    ||| ("-" ~ "(") ~> binaryexpression <~ ")" ^^ { case exp => UnaryExpression("-", exp) }
    ||| "-" ~> binaryexpression ^^ { case exp => UnaryExpression("-", exp) }
    ||| operatorApplication
    ||| locationize("-".? ~ numericLit ^^ { case s ~ n => if (isInt(s.getOrElse("") + n)) IntegerConstant((s.getOrElse("") + n).toInt) else FloatConstant((s.getOrElse("") + n).toDouble) })
    ||| ident ^^ { case id => Access(id) })

  lazy val operatorApplication : PackratParser[OperatorApplication] =
    (ident <~ "(") ~ (binaryexpression <~ ")") ^^ { case id ~ exp => OperatorApplication(id, exp) }
}
