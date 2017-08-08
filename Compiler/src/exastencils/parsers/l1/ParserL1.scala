package exastencils.parsers.l1

import scala.io.Source
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import exastencils.base.l1._
import exastencils.parsers.l1._

/** Main parser of the L1-compiler implemented as token-parser
 *
 *  Use parseFile() to parse the complete L1-code and get the first stage data-structure
 *
 *  This object contains all the grammatical rules for each section
 *  and uses the same lexical parser for each (see L1Lexer.scala).
 *
 *  The PackratParser-trait is used for left recursive parsing of mathematical syntax
 *
 *  Numeric values are (un-)signed integers and real numbers
 *  Mathematical expressions are parsed based on a tree-structure (see dataStructure.MathTreeExpr.scala)
 *  */
object L1_Parser extends StandardTokenParsers with PackratParsers{

  // left recurisve parser types
  type BParser = PackratParser[BoundaryTree]
  type IParser = PackratParser[IntervalLike]
  type MParser = PackratParser[MathTree]


  override val lexical = L1_Lexer

  /** parsing based on a file */
  def parseFile(file: String): FirstStageData = {
    val l1Code = Source.fromFile(file).getLines.reduceLeft(_+"\n"+_)
    parseCode(l1Code)
  }

  /** parsing based on a string */
  def parseCode(l1Code: String): FirstStageData = {
    val sections = L1_SectionParser.parse(l1Code)
    def getSection(name: String) = {
      if (sections contains name)
        sections(name)
      else
        throw new Exception("Section \""+name+"\"not found")
    }
    if (!(sections contains "Domain")) throw new RuntimeException("Missing Domain section")
    if (!(sections contains "Equation")) throw new RuntimeException("Missing Equation section")
    if (!(sections contains "Boundaries")) throw new RuntimeException("Missing Boundaries section")

    val dom         = parseSec(domain,     getSection("Domain"))
    val eq          = parseSec(equation,   getSection("Equation"))
    val boundary    = parseSec(boundaries, getSection("Boundaries"))

    val funcs : Map[String,UserFunction] = (
      if (sections contains "Functions")
        parseSec(functions, getSection("Functions"))
      else
        Map()
      )
    val params = (
      if (sections contains "Options")
        new FddParameters(dom, parseSec(options, getSection("Options")))
      else
        new FddParameters(dom, List())
      )

    new FirstStageData(dom,eq,boundary,funcs,params)
  }

  /** parsing of single individual sections, based on a string */
  def parseSec[T](parser: Parser[T], code: String): T = {
    val scanner = new lexical.Scanner(code)
    phrase(parser)(scanner) match {
      case Success(output,_) => output;
      case Error(msg,_) => throw new Exception("parse errror: " + msg)
      case Failure(msg, parser) => {
        var sb = new StringBuilder
        sb.append(s"Parse failure at position ${parser.pos}: $msg\n")
        sb.append(parser.pos.longString)
        sb.append("\n")
        throw new Exception(sb.toString)
      }
    }
  }

  /** implicit conversion for (latex, unicode)string-pairs */
  implicit def stringPairToParser(pair: (String,String)) = pair._1 | pair._2

  //////////////////////////////
  //    Parser Definitions    //
  //////////////////////////////

  // Domain Parsing //
  lazy val domain = (
    omega ~> opt(":") ~> "=" ~> cartPower ~ rep(ReservedSigns.times ~> cartPower)
      ^^ { case dom1 ~ Nil     => dom1
    case dom1 ~ domList => dom1 + domList.reduceLeft( (x,y) => x + y) }
    )
  lazy val omega = ReservedNames.domain
  lazy val cartPower = (
    interval ~  opt(("^"|"^^"|"**") ~> uInteger) ^^
      { case interval ~ None      => new Domain(interval)
      case interval ~ Some(exp) => new Domain(List.fill(exp)(interval)) }
    )
  lazy val interval = (
    intervalStart ~ sDouble ~ "," ~ sDouble ~ intervalEnd
      ^^ { case x0Prop ~ x0 ~ _ ~ x1 ~ x1Prop => Interval(x0, x0Prop, x1, x1Prop) }
    )
  lazy val intervalStart = (
    ("]" | "(")
      ^^ { case _ => false}
      | ("[")
      ^^ { case _  => true }
    )
  lazy val intervalEnd = (
    ("[" | ")")
      ^^ { case _ => false }
      | ("]")
      ^^ { case _ => true }
    )

  // Boundary Parsing //
  lazy val boundaries = (
    boundary ~ rep(";" ~> boundary) <~ opt(";")
      ^^ { case bound ~ Nil => List(bound)
    case bound ~ bounds => bound :: bounds }
    )
  lazy val boundary = (
    boundaryFunc ~ (ReservedNames.boundaryKey ~> boundaryPart)
      ^^ { case cond ~ part => Boundary(cond,part) }
    )
  lazy val boundaryFunc = equation ^^ { case eq => BoundaryCondition(eq) }
  lazy val boundaryPart = whole ||| boundaryL1
  lazy val whole = partDiff ~> "\\Omega" ^^ { _ => Whole() }

  lazy val boundaryL1: BParser = logicalOr ||| boundaryL2
  lazy val boundaryL2: BParser = logicalAnd ||| boundaryL3
  lazy val boundaryL3: BParser = (
    inequalities ||| intervalsForDim ||| "(" ~> boundaryL1 <~ ")"
    )

  lazy val logicalOr: BParser = (
    boundaryL1 ~ ReservedSigns.logicalOr ~ boundaryL2
      ^^ { case left ~ _ ~ right => Or(left,right) }
    )
  lazy val logicalAnd: BParser = (
    boundaryL2 ~ ReservedSigns.logicalAnd ~ boundaryL3
      ^^ { case left ~ _ ~ right => And(left,right) }
    )

  lazy val inequalities: BParser = (
    mathL1 ~ rep1(relation ~ mathL1)
      ^^ { case expr1 ~ list => ParserHelper.parseInequalites(expr1, list) } //TODO
    )
  lazy val relation: Parser[Relation] =
    ( "=" ^^ { case _ => EQ }
      | "<" ^^ { case _ => LESS }
      | ">" ^^ { case _ => GREATER }
      | ReservedSigns.lessOrEqual
      ^^ { case _ => LESSEQ }
      | ReservedSigns.greaterOrEqual
      ^^ { case _ => GREATEREQ }
      )

  lazy val intervalsForDim: BParser = (
    inIntervalsFor | exIntervalsFor
    )
  lazy val inIntervalsFor: BParser = (
    vecComps ~ ReservedSigns.elemOf ~ intervalsL1
      ^^ {case indices ~ _ ~ intervals =>
      indices.map{IntervalAtDim(_,intervals,true)}
        .reduceLeft[BoundaryTree]{(left,right) => And(left,right)}
    }
    )
  lazy val exIntervalsFor: BParser = (
    vecComps ~ ReservedSigns.notElemOf ~ intervalsL1
      ^^ { case indices ~ _ ~ intervals =>
      indices.map{IntervalAtDim(_,intervals,false)}
        .reduceLeft[BoundaryTree]{ (int1,int2) => And(int1,int2) } }
    )
  lazy val vecComps = (
    index(ReservedNames.vector)
      ^^ { case i => List(i) } |
      (ReservedNames.vector ~> "_" ~> "{" ~> uInteger) ~ (rep("," ~> uInteger) <~ "}")
        ^^ { case head ~ tail => head::tail }
    )

  lazy val intervalsL1 : IParser = setMinus | intervalsL2
  lazy val intervalsL2 : IParser = intersection | intervalsL3
  lazy val intervalsL3 : IParser = union | intervalsL4
  lazy val intervalsL4 : IParser = interval

  lazy val setMinus = (
    intervalsL1 ~ ReservedSigns.setMinus ~ intervalsL2
      ^^ { case left ~ _ ~ right => SetMinus(left,right)}
    )
  lazy val intersection = (
    intervalsL2 ~ ReservedSigns.intersection ~ intervalsL3
      ^^ { case left ~ _ ~ right => Intersection(left,right) }
    )
  lazy val union = (
    intervalsL3 ~ ReservedSigns.union ~ intervalsL4
      ^^ { case left ~ _ ~ right => Union(left,right) }
    )

  // Option Parsing //
  lazy val options = (
    option ~ rep(";" ~> option) <~ opt(";")
      ^^ { case opt ~ Nil => List(opt)
    case opt ~ opts => opt :: opts }
    )

  lazy val option = errorOrder | gridPoints | fdDirection
  lazy val errorOrder = (
    index(ReservedNames.errorOrder) ~ ("=" ~> uInteger)
      ^^ { case dim ~ value => ErrorOrder(dim-1,value) }
    )
  lazy val gridPoints = (
    index(ReservedNames.gridPoints) ~ ("=" ~> uInteger)
      ^^ { case dim ~ value => GridPoints(dim-1,value) }
    )
  lazy val fdDirection = (
    partDiff ~> subScript(uInteger) ~ ("=" ~> sInteger)
      ^^ { case dim ~ 1  => Direction(dim-1, FORWARD)
    case dim ~ 0  => Direction(dim-1, CENTRAL)
    case dim ~ -1 => Direction(dim-1, BACKWARD) }
    )

  // Math Tree Parsing //
  lazy val equation = (
    mathL1 ~ "=" ~ mathL1
      ^^ { case left~"="~right => Equation(left,right) }
    )
  lazy val functions = (
    function ~ rep(";" ~> function) <~ opt(";")
      ^^ { case func ~ Nil   => ParserHelper.parseFuncs(List(func))
    case func ~ funcs => ParserHelper.parseFuncs(func :: funcs) }
    )
  lazy val function = (
    ident ~ ("(" ~> arguments <~ ")") ~ (opt(":") ~> "=" ~> mathL1)
      ^^ { case name ~ args ~ expr => (name -> new UserFunction(name, args, expr)) }
    )
  lazy val arguments = (
    argument ~ rep("," ~> argument)
      ^^ { case arg ~ args => arg::args }
    )
  lazy val argument = ident ||| ReservedNames.solution ||| ReservedNames.vector
  lazy val mathL1: MParser = add ||| sub ||| mathL2
  lazy val mathL2: MParser = mul ||| div ||| mathL3
  lazy val mathL3: MParser = exp ||| mathL4
  lazy val mathL4: MParser = (
    dev ||| solution ||| value |||  vectorEntry ||| vector |||
      variable ||| functionCall ||| "(" ~> mathL1 <~ ")"
    )

  // Basic Arithmetic Parsing //
  lazy val add: MParser = mathL1 ~ ("+" ~> mathL2) ^^ { case left~right => Add(left,right) }
  lazy val sub: MParser = mathL1 ~ ("-" ~> mathL2) ^^ { case left~right => Sub(left,right) }
  lazy val mul: MParser = mathL2 ~ (("*"|ReservedSigns.dotOperator) ~> mathL3) ^^ { case left~right => Mul(left,right) }
  lazy val div: MParser = mathL2 ~ ("/" ~> mathL3) ^^ { case left~right => Div(left,right) }
  lazy val exp: MParser = mathL3 ~ (("**"|"^^"|"^") ~> mathL4) ^^ { case base ~ power => Exp(base,power) }

  lazy val value: MParser = sDouble ^^ { t => Value(t) }
  lazy val solution: MParser = ReservedNames.solution ^^ { _ => Solution() }
  lazy val vectorEntry: MParser = index(ReservedNames.vector) ^^ { index => VecEntry(index) }
  lazy val vector: MParser = ReservedNames.vector ^^ { _ => Vec() }
  lazy val variable: MParser = ident ^^ { Variable(_) }
  lazy val functionCall: MParser = (
    ident ~ ("(" ~> mathL1) ~ (rep("," ~> mathL1) <~")")
      ^^ { case name ~ arg ~ args => FunctionCall(name,arg::args) }
    )

  // Derivation(dev) Parsing //
  lazy val dev = ( devOp |||
    partDiff ~> subScript( devDim ~ rep( "," ~> devDim ) ) ~ ReservedNames.solution
      ^^ { case devDim ~ Nil ~ _ => NestedDerivation(List(devDim))
    case devDim ~ devDims ~ _ => NestedDerivation(devDim :: devDims) }
    )
  lazy val devDim = (
    index(ReservedNames.vector) ~ superScript(uInteger)
      ^^ { case dim ~ count => Derivation(dim-1,count) }
    )

  lazy val partDiff = "\\partial" | "\u2202"

  lazy val devOp     = normalDev | laplace
  lazy val normalDev = partDiff ~> subScript( ReservedNames.normal ) ~> ReservedNames.solution ^^ { _ => NormalDerivation() }
  lazy val laplace   = ReservedSigns.capitalDelta ~> (ReservedNames.solution | "("~>ReservedNames.solution<~ ")") ^^ { _ => Laplace() }

  // Index parsing //
  def index (ident: String) = ident ~> subScript(uInteger)

  // SubScript Parsing //
  def subScript[U] (p: L1_Parser.Parser[U]) = "_"~>(p|"{"~>p<~ "}")
  // SuperScript Parsing //
  def superScript[U] (p: L1_Parser.Parser[U]) = "^"~>(p|"{"~>p<~ "}")

  // Numeric Parsing //
  // unsigned real number (no leading optSign)
  lazy val uDouble = numericLit ~ ("." ~ numericLit).? ^^ { case intPart ~ Some(dot ~ fracPart) => (intPart+dot+fracPart).toDouble
  case intPart ~ None => intPart.toDouble }
  // signed real number
  lazy val sDouble = optSign ~ numericLit ~ ("." ~ numericLit).? ^^ { case sig ~ intPart ~ Some(dot ~ fracPart) => (sig+intPart+dot+fracPart).toDouble
  case sig ~ intPart ~ None => (sig+intPart).toDouble }
  // unsigned integer (no leading optSign)
  lazy val uInteger = numericLit ^^ { case digits => digits.toInt }
  // signed integer
  lazy val sInteger = optSign ~ numericLit ^^ { case sign ~ digits => (sign + digits).toInt }
  // optional sign
  lazy val optSign: Parser[String] = ("+"|"-").?  ^^ { case Some(sign) => sign; case None => "" }

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
    val tailSection = """([\s\S]*)\s*\n\s*([A-Z][a-z]*):\s*\n([\s\S]*)""".r // pattern matching the last section

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

object ParserHelper {

  //  /** Helper function parsing the Derivation, used for clarity.
  //   *
  //   *  @throws  An exception if 'total' doesn't match the total number of derivations
  //   *
  //   *  @param  total  Total number of derivations in each direction
  //   *  @param  devs   Derivations
  //   *
  //   */
  //  def parseDev(total: Int, devs: Seq[Derivation]) : Differentiable = {
  //    if (total != devs.foldLeft(0){ (number,dev) => number+dev.number } ) {
  //      throw new RuntimeException("Numbers of derivatives does not match up")
  //    } else if (devs.size == 1)
  //      devs.head
  //    else
  //      NestedDerivation(devs)
  //  }

  /** Helper function parsing the user functions.
   *
   *  @throws  An exception if multiple definitions for one function name exist
   *
   *  @param  funcs  List of Name-UserFunction pairs
   *
   */
  def parseFuncs(funcs: List[(String,UserFunction)]) =  {
    val funcsByName =
      funcs.foldLeft( Map[String,List[UserFunction]]() ) {
        (map,pair) =>
        {
          if (map.contains(pair._1))
            map.updated(pair._1, pair._2 :: map(pair._1))
          else
            map.updated(pair._1, List(pair._2))
        }
      }
    funcsByName find {_._2.size > 1} match {
      case Some((name,defs)) => throw new RuntimeException(s"multiple definitions for function $name")
      case None => funcsByName mapValues { _(0) }
    }
  }

  /** Helper function parsing inequality chains as BoundaryTree.
   *
   *  @param  expr1  Leftmost side of the inequalities
   *  @param  list   List of parsed relations with their left side expressions
   *
   */
  def parseInequalites(expr1: MathTree, list: List[L1_Parser.~[Relation,MathTree]]) = {
    // unzip list
    val (relations, exprs) = list.map{x => (x._1, x._2)}.unzip
    // zip left expr with relation with right expr
    val inequalites = (expr1::exprs) zip relations zip (exprs) map {
      // map to Inequality
      case ((left,relation),right) => Inequality(left,right,relation)
    }
    // reduce with And()
    inequalites.reduceLeft[BoundaryTree]{(x,y) => And(x,y)}
  }

}

