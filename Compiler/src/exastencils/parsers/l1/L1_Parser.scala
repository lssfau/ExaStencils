package exastencils.parsers.l1

import scala.io.Source
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import exastencils.base.l1._

/** Main parser of the L1-compiler implemented as token-parser
  *
  * Use parseFile() to parse the complete L1-code and get the first stage data-structure
  *
  * This object contains all the grammatical rules for each section
  * and uses the same lexical parser for each (see L1Lexer.scala).
  *
  * The PackratParser-trait is used for left recursive parsing of mathematical syntax
  *
  * Numeric values are (un-)signed integers and real numbers
  * Mathematical expressions are parsed based on a tree-structure (see dataStructure.MathTreeExpr.scala)
  * */
object L1_Parser extends StandardTokenParsers with PackratParsers {

  type MParser = PackratParser[L1_MathTree] // used for left-recursive parsing

  override val lexical = L1_Lexer

  /** parsing based on a file */
  def parseFile(file : String) : L1_FirstStageData = {
    val l1Code = Source.fromFile(file).getLines.reduceLeft(_ + "\n" + _)
    parseCode(l1Code)
  }

  /** parsing based on a string */
  def parseCode(l1Code : String) : L1_FirstStageData = {
    val sections = L1_SectionParser.parse(l1Code)

    def getSection(name : String) = {
      if (sections contains name)
        sections(name)
      else
        throw new Exception("Section \"" + name + "\"not found")
    }

    val dom : L1_Domain = parseSec(domain, getSection("Domain"))
    val eq : L1_Equation = parseSec(equation, getSection("Equation"))

    new L1_FirstStageData(dom, eq)
  }

  /** parsing of single individual sections, based on a string */
  def parseSec[T](parser : Parser[T], code : String) : T = {
    val scanner = new lexical.Scanner(code)
    phrase(parser)(scanner) match {
      case Success(output, _)   => output;
      case Error(msg, _)        => throw new Exception("parse errror: " + msg)
      case Failure(msg, parser) => {
        var sb = new StringBuilder
        sb.append(s"Parse failure at position ${ parser.pos }: $msg\n")
        sb.append(parser.pos.longString)
        sb.append("\n")
        throw new Exception(sb.toString)
      }
    }
  }

  /** Helper Function parsing the Derivation, used for clarity.
    *
    * @throws Exception if 'total' doesn't match the total number of derivations
    * @param  total Total number of derivations in each direction
    * @param  devs  Derivations
    *
    */
  def parseDev(total : Int, devs : Seq[L1_Derivation]) : L1_Differentiable = {
    if (total != devs.foldLeft(0) { (number, dev) => number + dev.number }) {
      println(s"devs: $devs")
      println(s"total: $total")
      throw new Exception("Numbers of derivatives does not match up")
    } else if (devs.size == 1)
      devs.head
    else
      L1_NestedDerivation(devs)
  }

  //////////////////////////////
  //    Parser Definitions    //
  //////////////////////////////

  // Domain Parsing //
  lazy val domain = cartPower ~ rep("\\times" ~> cartPower) ^^ { case dom1 ~ domList => dom1 + domList.reduceLeft(_ + _) }
  lazy val cartPower : Parser[L1_Domain] = dim ~ opt(("^^" | "**") ~> uInteger) ^^ { case dim ~ Some(exp) => new L1_Domain(Seq.fill(exp)(dim))
  case dim ~ None                                                                                         => new L1_Domain(dim)
  }
  lazy val dim : Parser[L1_Dimension] = ("[" ~> sDouble) ~ ("," ~> sDouble <~ "]") ^^ { case db1 ~ db2 => L1_Dimension(db1.toDouble, db2.toDouble) }

  // Math Tree Parsing //
  lazy val equation = mathLevel1 ~ "=" ~ mathLevel1 ^^ { case left ~ "=" ~ right => L1_Equation(left, right) }
  lazy val mathLevel1 : MParser = add ||| sub ||| mathLevel2
  lazy val mathLevel2 : MParser = mul ||| div ||| mathLevel3
  lazy val mathLevel3 : MParser = (dev | exp) ||| mathLevel4
  lazy val mathLevel4 : MParser = value ||| func ||| "(" ~> mathLevel1 <~ ")"

  // Basic Arithmetic Parsing //
  lazy val add : MParser = mathLevel1 ~ ("+" ~> mathLevel2) ^^ { case left ~ right => L1_Addition(left, right) }
  lazy val sub : MParser = mathLevel1 ~ ("-" ~> mathLevel2) ^^ { case left ~ right => L1_Subtraction(left, right) }
  lazy val mul : MParser = mathLevel2 ~ ("*" ~> mathLevel3) ^^ { case left ~ right => L1_Multiplication(left, right) }
  lazy val div : MParser = mathLevel2 ~ ("/" ~> mathLevel3) ^^ { case left ~ right => L1_Division(left, right) }
  lazy val exp : MParser = mathLevel3 ~ (("**" | "^^") ~> value) ^^ { case base ~ power => L1_Exponential(base, power) }
  lazy val value = sDouble ^^ { case t => L1_Value(t) }
  lazy val func = "U" ^^ { case _ => L1_Function() }

  // Derivation(dev) Parsing //
  lazy val dev = devOp ||| (devTop <~ "U" <~ "/") ~ devBot ^^ { case total ~ dev => parseDev(total, Seq(dev)) } |
    (devTop <~ "U" <~ "/") ~ ("(" ~> rep1(devBot) <~ ")") ^^ { case total ~ seqDev => parseDev(total, seqDev) }
  lazy val devTop = partDiff ~> devCount
  lazy val devBot = (partDiff ~> "x" ~> "_" ~> uInteger) ~ devCount ^^ { case dir ~ t => L1_Derivation(dir, t) }
  lazy val devCount = opt("^" ~> uInteger) ^^ { case None => 1; case Some(t) => t }
  lazy val partDiff = "\\partial" | "\u2202"

  lazy val devOp = laplace
  lazy val laplace = ("\u0394" | "\u2206" | "\\Delta") ~> ("U" | "(" ~> "U" <~ ")") ^^ { _ => L1_Laplace() }

  // Numeric Parsing //
  // unsigned real number (no leading optSign)
  lazy val uDouble = numericLit ~ ("." ~ numericLit).? ^^ { case intPart ~ Some(dot ~ fracPart) => (intPart + dot + fracPart).toDouble
  case intPart ~ None                                                                           => intPart.toDouble
  }
  // signed real number
  lazy val sDouble = optSign ~ numericLit ~ ("." ~ numericLit).? ^^ { case sig ~ intPart ~ Some(dot ~ fracPart) => (sig + intPart + dot + fracPart).toDouble
  case sig ~ intPart ~ None                                                                                     => (sig + intPart).toDouble
  }
  // unsigned integer (no leading optSign)
  lazy val uInteger = numericLit ^^ { case digits => digits.toInt }
  // signed integer
  lazy val sInteger = optSign ~ numericLit ^^ { case sign ~ digits => (sign + digits).toInt }
  // optional sign
  lazy val optSign : Parser[String] = ("+" | "-").? ^^ { case Some(sign) => sign; case None => "" }

}

/** Splits the L1-Code in sections based on regular expressions of the form:
  *
  * <SectionName1>:\n
  * <SectionCode1>
  * ...
  * <SetionNameN>:\n
  * <SetionCodeN>
  *
  * Where <SectinName> must begin with an upper-case letter and end with a colon and a new line
  * <SectionCode1> then can be any text (except an SectionName) across an arbitrary number of lines
  * */
object L1_SectionParser {

  /** Invokes the section splitting
    *
    * @param l1Code L1-Code as String (including new lines)
    * @return a Map with the section names as keys and the section bodies as values
    * */
  def parse(l1Code : String) : Map[String, String] = {
    val headSection = """\s*([A-Z][a-z]*):\s*\n([\s\S]*)""".r // pattern matching the first section
    val tailSection =
      """([\s\S]*)\s*\n\s*([A-Z][a-z]*):\s*\n([\s\S]*)""".r // pattern matching the last section

    def splitRec(code : String) : Map[String, String] = {
      code match {
        case tailSection(rest, name, content) => splitRec(rest) ++ Map(name -> content)
        case headSection(name, content)       => Map(name -> content)
        case _                                => throw new Exception("code does not match form:\nSectionName1:\nCode1...\nSectionName2:\nCode2...\n...")
      }
    }
    splitRec(l1Code)
  }
}

//
////object oldL1_Parser extends ExaParser {
////  override val lexical = L1_Lexer
////
////  def parseFile(filename : String) : Node = {
////    val lines = io.Source.fromFile(filename).getLines
////    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
////    val scanner = new lexical.Scanner(reader)
////    parseTokens(scanner)
////  }
////
////  protected def parseTokens(tokens : lexical.Scanner) : Node = {
////    phrase(program)(tokens) match {
////      case Success(e, _) => e
////      case Error(msg, _) => throw new Exception("parse error: " + msg)
////      case Failure(msg, parser) => {
////        var sb = new StringBuilder
////        sb.append(s"Parse failure at position ${parser.pos}: $msg\n")
////        sb.append(parser.pos.longString)
////        sb.append("\n")
////        throw new Exception(sb.toString)
////      }
////    }
////  }
////
////  // ######################################
////  // ##### basic definitions
////  // ######################################
////
////  lazy val program = (domain ||| operator ||| equation ||| rhs ||| mapping).+ ^^ { case x => L1_Root(x) }
////
////  lazy val domain = ("Domain" ~> ident) ~ ("=" ~> range) ^^ { case id ~ range => L1_Domain(id, range) }
////  lazy val operator = ("Operator" ~> ident) ~ ("=" ~> binaryexpression) ^^ { case id ~ exp => L1_Operator(id, exp) }
////  lazy val equation = ("Equation" ~> binaryexpression) ~ ("=" ~> binaryexpression) ^^ { case l ~ r => L1_Equation(l, r) }
////  lazy val rhs = ("RHS" ~> ident) ~ ("=" ~> binaryexpression) ^^ { case id ~ exp => L1_RHS(id, exp) }
////  lazy val mapping = ("Mapping" ~> ident) ~ (arrow ~> set) ^^ { case id ~ set => L1_Mapping(id, set) }
////
////  // ######################################
////  // ##### range definitions
////  // ######################################
////
////  lazy val range = range1d ||| range2d ||| range3d
////
////  lazy val range1d = ("[" ~> realLit <~ ",") ~ (realLit <~ "]") ^^ { case a ~ b => List(Tuple2(a, b)) }
////  lazy val range2d = (range1d <~ rangeMultiply) ~ range1d ^^ { case a ~ b => a ::: b }
////  lazy val range3d = (range2d <~ rangeMultiply) ~ range1d ^^ { case a ~ b => a ::: b }
////  lazy val rangeMultiply = "\\times" ||| "\u00D7" ||| "*"
////
////  // ######################################
////  // ##### mapping definitions
////  // ######################################
////
////  lazy val arrow = "-" ~ ">"
////  lazy val set = (
////    ("C" ||| "R") ~ ("^" ~> ("1" ||| "2" ||| "3")) ^^ { case id ~ exp => L1_MathSet(id, exp) }
////      ||| ident ^^ { case id => L1_MathSet(id) })
////
////  // ######################################
////  // ##### binary expressions
////  // ######################################
////
////  lazy val binaryexpression : PackratParser[L1_Expression] = (
////    ((binaryexpression ~ ("+" ||| "-") ~ term) ^^ { case lhs ~ op ~ rhs => L1_BinaryExpression(op, lhs, rhs) })
////    ||| term)
////
////  lazy val term : PackratParser[L1_Expression] = (
////    ((term ~ ("*" ||| "/") ~ term2) ^^ { case lhs ~ op ~ rhs => L1_BinaryExpression(op, lhs, rhs) })
////    ||| term2)
////
////  lazy val term2 : PackratParser[L1_Expression] = (
////    ((term2 ~ ("**" ||| "^") ~ factor) ^^ { case lhs ~ op ~ rhs => L1_BinaryExpression(op, lhs, rhs) })
////    ||| factor)
////
////  lazy val factor : PackratParser[L1_Expression] = (
////    "(" ~> binaryexpression <~ ")"
////    ||| "{" ~> binaryexpression <~ "}"
////      ||| ("-" ~ "(") ~> binaryexpression <~ ")" ^^ { case exp => L1_UnaryExpression("-", exp) }
////      ||| "-" ~> binaryexpression ^^ { case exp => L1_UnaryExpression("-", exp) }
////    ||| operatorApplication
////      ||| locationize("-".? ~ numericLit ^^ { case s ~ n => if (isInt(s.getOrElse("") + n)) L1_IntegerConstant((s.getOrElse("") + n).toInt) else L1_FloatConstant((s.getOrElse("") + n).toDouble) })
////      ||| ident ^^ { case id => L1_Access(id) })
////
////  lazy val operatorApplication : PackratParser[L1_OperatorApplication] =
////    (ident <~ "(") ~ (binaryexpression <~ ")") ^^ { case id ~ exp => L1_OperatorApplication(id, exp) }
////}
