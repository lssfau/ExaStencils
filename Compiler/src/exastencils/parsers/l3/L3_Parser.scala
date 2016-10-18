package exastencils.parsers.l3

import scala.collection.immutable.PagedSeq
import scala.collection.mutable._
import scala.io._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.parsers._

object L3_Parser extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = L3_Lexer

  def parse(s : String) : L3_Root = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : L3_Root = {
    val lines = Source.fromFile(filename).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    parseTokens(scanner)
  }

  protected def parseTokens(tokens : lexical.Scanner) : L3_Root = {
    phrase(program)(tokens) match {
      case Success(e, _)        => e
      case Error(msg, _)        => throw new Exception("parse error: " + msg)
      case Failure(msg, parser) =>
        val sb = new StringBuilder
        sb.append(s"Parse failure at position ${ parser.pos }: $msg\n")
        sb.append(parser.pos.longString)
        sb.append("\n")
        throw new Exception(sb.toString)
    }
  }

  //###########################################################

  lazy val program = ((
    function
    ).*
    ^^ (nodes => L3_Root(nodes)))

  //###########################################################

  // #############################################################################
  // #################################### BASE ###################################
  // #############################################################################

  // ######################################
  // ##### L3_BinaryOps, L3_Constant
  // ######################################

  lazy val binaryexpression : PackratParser[L3_Expression] = (
    locationize((binaryexpression ~ ("+" ||| "-" ||| ".+" ||| ".-") ~ term) ^^ { case lhs ~ op ~ rhs => L3_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term)

  lazy val term : PackratParser[L3_Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "%" ||| ".*" ||| "./" ||| ".%") ~ term2) ^^ { case lhs ~ op ~ rhs => L3_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term2)

  lazy val term2 : PackratParser[L3_Expression] = (
    locationize((term2 ~ ("**" ||| "^" ||| ".**") ~ factor) ^^ { case lhs ~ op ~ rhs => L3_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| factor)

  lazy val factor = (
    "(" ~> binaryexpression <~ ")"
      ||| ("-" ~ "(") ~> binaryexpression <~ ")" ^^ { exp => L3_UnaryOperators.createExpression("-", exp) }
      ||| locationize(stringLit ^^ { L3_StringConstant })
      ||| locationize("-".? ~ numericLit ^^ { case s ~ n =>
      if (isInt(s.getOrElse("") + n)) L3_IntegerConstant((s.getOrElse("") + n).toInt)
      else L3_RealConstant((s.getOrElse("") + n).toDouble)
    })
      ||| locationize("-" ~> functionCall ^^ { x => L3_UnaryOperators.createExpression("-", x) })
      ||| functionCall
      ||| locationize("-" ~> genericAccess ^^ { x => L3_UnaryOperators.createExpression("-", x) })
      ||| genericAccess
      ||| locationize(booleanLit ^^ { L3_BooleanConstant }))

  lazy val booleanexpression : PackratParser[L3_Expression] = (
    locationize((booleanexpression ~ ("||" ||| "or") ~ booleanexpression1) ^^ { case ex1 ~ op ~ ex2 => L3_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression1)

  lazy val booleanexpression1 : PackratParser[L3_Expression] = (
    locationize((booleanexpression1 ~ ("&&" ||| "and") ~ booleanexpression2) ^^ { case ex1 ~ op ~ ex2 => L3_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression2)

  lazy val booleanexpression2 : PackratParser[L3_Expression] = (
    locationize(("!" ~> booleanexpression3) ^^ { ex => L3_UnaryOperators.createExpression("!", ex) })
      ||| booleanexpression3)

  lazy val booleanexpression3 : PackratParser[L3_Expression] = (
    "(" ~> booleanexpression <~ ")"
      ||| comparison
      ||| binaryexpression)

  lazy val comparison : PackratParser[L3_Expression] = //(
    locationize((binaryexpression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ binaryexpression) ^^ { case ex1 ~ op ~ ex2 => L3_BinaryOperators.createExpression(op, ex1, ex2) })

  // ######################################
  // ##### L3_Datatype
  // ######################################

  lazy val datatype : Parser[L3_Datatype] = (
    simpleDatatype
      ||| algorithmicDatatype
      ||| higherOrderDatatype)

  lazy val simpleDatatype : Parser[L3_Datatype] = (
    ("String" ||| "string") ^^ { _ => L3_StringDatatype }
      ||| ("Boolean" ||| "boolean" ||| "Bool" ||| "bool") ^^ { _ => L3_BooleanDatatype }
      ||| numericDatatype)

  lazy val algorithmicDatatype : Parser[L3_Datatype] = (
    (("Complex" ||| "complex") ~ "<") ~> numericDatatype <~ ">" ^^ { L3_ComplexDatatype }
      ||| numericDatatype)

  lazy val numericDatatype : Parser[L3_Datatype] = (
    ("Integer" ||| "integer" ||| "Int" ||| "int") ^^ { _ => L3_IntegerDatatype }
      ||| ("Real" ||| "real") ^^ { _ => L3_RealDatatype }
      ||| ("Float" ||| "float") ^^ { _ => L3_FloatDatatype }
      ||| ("Double" ||| "double") ^^ { _ => L3_DoubleDatatype })

  lazy val returnDatatype = (("Unit" ||| "unit") ^^ { _ => L3_UnitDatatype }
    ||| datatype)

  // ######################################
  // ##### L3_Function
  // ######################################

  lazy val function = locationize((("Func" ||| "Function") ~> ident) ~ level.? ~ ("(" ~> functionArgumentList.? <~ ")").? ~ (":" ~> returnDatatype).? ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case name ~ levels ~ args ~ returnType ~ stmts => L3_Function(name, levels, returnType, args, stmts) })
  lazy val functionArgumentList = /*locationize*/ (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => args :+ arg }
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => L3_FunctionArgument(id, t) })

  lazy val returnStatement = locationize("return" ~> (binaryexpression ||| booleanexpression).? ^^ { L3_Return })

  lazy val functionCallArgumentList = /*locationize*/ ((binaryexpression ||| booleanexpression) <~ ("," | newline)).* ~ (binaryexpression ||| booleanexpression) ^^ { case exps ~ ex => exps :+ ex }
  lazy val functionCall = locationize(genericAccess ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => L3_FunctionCall(id, args) })

  // ######################################
  // ##### L3_LevelSpecification
  // ######################################

  lazy val level = (
    locationize("@" ~> (levelsingle ||| levelall) ^^ { l => l })
      ||| locationize("@" ~ "(" ~> levellist <~ ")" ^^ { l => l }))

  lazy val levelAccess = (
    locationize("@" ~> levelsingle ^^ { l => l })
      ||| locationize("@" ~ "(" ~> levelsingle <~ ")" ^^ { l => l }))

  lazy val levellist = locationize(((levelall ||| levelsingle ||| levelrange ||| levelrelative ||| levelnegation) <~ ("," ||| "and")).* ~ (levelall ||| levelsingle ||| levelrange ||| levelrelative ||| levelnegation)
    ^^ { case a ~ b => L3_LevelList(a :+ b) })

  lazy val levelsublist = locationize(((levelsingle ||| levelrange ||| levelrelative) <~ ("," ||| "and")).* ~ (levelsingle ||| levelrange ||| levelrelative)
    ^^ { case a ~ b => L3_LevelList(a :+ b) })

  lazy val levelnegation = (locationize((("not" ||| "but") ~ "(") ~> levelsublist <~ ")" ^^ { l => L3_NegatedLevelList(l) })
    ||| locationize(("not" ||| "but") ~> levelsingle ^^ { l => L3_NegatedLevelList(l) }))

  lazy val levelrange = locationize((levelsingle ||| "(" ~> levelrelative <~ ")") ~ "to" ~ (levelsingle ||| "(" ~> levelrelative <~ ")")
    ^^ { case b ~ _ ~ e => L3_LevelRange(b, e) })

  lazy val levelrelative = locationize(levelsingle ~ ("+" ||| "-") ~ integerLit
    ^^ { case l ~ op ~ i => L3_RelativeLevel(l, op, i) })

  lazy val levelall = locationize("all" ^^ { _ => L3_AllLevels })

  lazy val levelsingle = (
    locationize("current" ^^ { _ => L3_CurrentLevel })
      ||| locationize("coarser" ^^ { _ => L3_CoarserLevel })
      ||| locationize("finer" ^^ { _ => L3_FinerLevel })
      ||| locationize("coarsest" ^^ { _ => L3_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L3_FinestLevel })
      ||| locationize(integerLit ^^ { L3_SingleLevel }))

  // ######################################
  // ##### L3_Statement
  // ######################################

  lazy val statement : Parser[L3_Statement] = (
//    localDeclaration
//      ||| generalAssignment
//      ||| conditional
//      ||| countLoop
//      ||| untilLoop
//      ||| whileLoop
    functionCall ^^ { L3_ExpressionStatement(_) }
      ||| returnStatement)

  // #############################################################################
  // ################################## BASE_EXT #################################
  // #############################################################################

  // ######################################
  // ##### L3_HigherOrderDatatype
  // ######################################

  lazy val higherOrderDatatype : Parser[L3_Datatype] = (("Array" ||| "array") ~> ("<" ~> datatype <~ ">") ~ ("<" ~> integerLit <~ ">")
    ^^ { case x ~ s => L3_ArrayDatatype(x, s) })

  // ######################################
  // ##### L3_UnresolvedAccess
  // ######################################

  lazy val genericAccess = locationize(ident ~ levelAccess.? ^^ { case id ~ level => L3_UnresolvedAccess(id, level) })

  // #############################################################################
  // ################################## L3_FIELD #################################
  // #############################################################################

}
