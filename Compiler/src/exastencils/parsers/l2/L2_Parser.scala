package exastencils.parsers.l2

import scala.collection.immutable.PagedSeq
import scala.collection.mutable._
import scala.io._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import exastencils.base.l2._
import exastencils.baseExt.l2._
import exastencils.boundary.l2._
import exastencils.domain.l2.L2_DomainDecl
import exastencils.field.l2._
import exastencils.operator.l2._
import exastencils.parsers._

object L2_Parser extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = L2_Lexer

  def parse(s : String) : L2_Root = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : L2_Root = {
    val lines = Source.fromFile(filename).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    parseTokens(scanner)
  }

  protected def parseTokens(tokens : lexical.Scanner) : L2_Root = {
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

  lazy val program = (
    domainDeclaration
      ||| fieldDeclaration
      ||| stencilDeclaration
      ||| stencilTemplateDeclaration).* ^^ { nodes => L2_Root(nodes) }

  //###########################################################

  // #############################################################################
  // #################################### BASE ###################################
  // #############################################################################

  // ######################################
  // ##### L2_BinaryOps
  // ######################################

  lazy val binaryexpression : PackratParser[L2_Expression] = (
    locationize((binaryexpression ~ ("+" ||| "-" ||| ".+" ||| ".-") ~ term) ^^ { case lhs ~ op ~ rhs => L2_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term)

  lazy val term : PackratParser[L2_Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "%" ||| ".*" ||| "./" ||| ".%") ~ term2) ^^ { case lhs ~ op ~ rhs => L2_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term2)

  lazy val term2 : PackratParser[L2_Expression] = (
    locationize((term2 ~ ("**" ||| "^" ||| ".**") ~ factor) ^^ { case lhs ~ op ~ rhs => L2_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| factor)

  lazy val factor = (
    "(" ~> binaryexpression <~ ")"
      ||| ("-" ~ "(") ~> binaryexpression <~ ")" ^^ { exp => L2_UnaryOperators.createExpression("-", exp) }
      ||| locationize(stringLit ^^ { s => L2_StringConstant(s) })
      ||| locationize("-".? ~ numericLit ^^ { case s ~ n =>
      if (isInt(s.getOrElse("") + n)) L2_IntegerConstant((s.getOrElse("") + n).toInt)
      else L2_RealConstant((s.getOrElse("") + n).toDouble)
    })
      ||| locationize("-" ~> functionCall ^^ { x => L2_UnaryOperators.createExpression("-", x) })
      ||| functionCall
      ||| locationize("-" ~> genericAccess ^^ { x => L2_UnaryOperators.createExpression("-", x) })
      ||| genericAccess
      ||| locationize(booleanLit ^^ { s => L2_BooleanConstant(s) }))

  lazy val booleanexpression : PackratParser[L2_Expression] = (
    locationize((booleanexpression ~ ("||" ||| "or") ~ booleanexpression1) ^^ { case ex1 ~ op ~ ex2 => L2_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression1)

  lazy val booleanexpression1 : PackratParser[L2_Expression] = (
    locationize((booleanexpression1 ~ ("&&" ||| "and") ~ booleanexpression2) ^^ { case ex1 ~ op ~ ex2 => L2_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression2)

  lazy val booleanexpression2 : PackratParser[L2_Expression] = (
    locationize(("!" ~> booleanexpression3) ^^ { ex => L2_UnaryOperators.createExpression("!", ex) })
      ||| booleanexpression3)

  lazy val booleanexpression3 : PackratParser[L2_Expression] = (
    "(" ~> booleanexpression <~ ")"
      ||| comparison
      ||| binaryexpression)

  lazy val comparison : PackratParser[L2_Expression] = //(
    locationize((binaryexpression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ binaryexpression) ^^ { case ex1 ~ op ~ ex2 => L2_BinaryOperators.createExpression(op, ex1, ex2) })

  // ######################################
  // ##### L2_Datatypes
  // ######################################

  lazy val datatype : Parser[L2_Datatype] = (
    simpleDatatype
      ||| algorithmicDatatype
      ||| higherOrderDatatype)

  lazy val simpleDatatype : Parser[L2_Datatype] = (
    ("String" ||| "string") ^^ { _ => L2_StringDatatype }
      ||| ("Boolean" ||| "boolean" ||| "Bool" ||| "bool") ^^ { _ => L2_BooleanDatatype }
      ||| numericDatatype)

  lazy val algorithmicDatatype : Parser[L2_Datatype] = (
    (("Complex" ||| "complex") ~ "<") ~> numericDatatype <~ ">" ^^ { x => L2_ComplexDatatype(x) }
      ||| numericDatatype)

  lazy val numericDatatype : Parser[L2_Datatype] = (
    ("Integer" ||| "integer" ||| "Int" ||| "int") ^^ { _ => L2_IntegerDatatype }
      ||| ("Real" ||| "real") ^^ { _ => L2_RealDatatype }
      ||| ("Float" ||| "float") ^^ { _ => L2_FloatDatatype }
      ||| ("Double" ||| "double") ^^ { _ => L2_DoubleDatatype })

  // ######################################
  // ##### L2_Function
  // ######################################

  lazy val functionCallArgumentList = /*locationize*/ ((binaryexpression ||| booleanexpression) <~ ("," | newline)).* ~ (binaryexpression ||| booleanexpression) ^^ { case exps ~ ex => exps :+ ex }
  lazy val functionCall = locationize(genericAccess ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => L2_FunctionCall(id, args) })

  // ######################################
  // ##### L2_Index
  // ######################################

  lazy val index = expressionIndex ||| constIndex

  lazy val expressionIndex = locationize("[" ~> binaryexpression ~ ("," ~> binaryexpression).* <~ "]" ^^ { case b ~ l => L2_ExpressionIndex((List(b) ++ l).toArray) })
  lazy val constIndex = locationize("[" ~> integerLit ~ ("," ~> integerLit).* <~ "]" ^^ { case b ~ l => L2_ConstIndex((List(b) ++ l).toArray) })

  // ######################################
  // ##### L2_LevelSpecification
  // ######################################

  lazy val level = (
    locationize("@" ~> (levelsingle ||| levelall) ^^ { l => l })
      ||| locationize("@" ~ "(" ~> levellist <~ ")" ^^ { l => l }))

  lazy val levelAccess = (
    locationize("@" ~> levelsingle ^^ { l => l })
      ||| locationize("@" ~ "(" ~> levelsingle <~ ")" ^^ { l => l }))

  lazy val levellist = locationize(((levelall ||| levelsingle ||| levelrange ||| levelrelative ||| levelnegation) <~ ("," ||| "and")).* ~ (levelall ||| levelsingle ||| levelrange ||| levelrelative ||| levelnegation)
    ^^ { case a ~ b => L2_LevelList(a :+ b) })

  lazy val levelsublist = locationize(((levelsingle ||| levelrange ||| levelrelative) <~ ("," ||| "and")).* ~ (levelsingle ||| levelrange ||| levelrelative)
    ^^ { case a ~ b => L2_LevelList(a :+ b) })

  lazy val levelnegation = (locationize((("not" ||| "but") ~ "(") ~> levelsublist <~ ")" ^^ { l => L2_NegatedLevelList(l) })
    ||| locationize(("not" ||| "but") ~> levelsingle ^^ { l => L2_NegatedLevelList(l) }))

  lazy val levelrange = locationize((levelsingle ||| "(" ~> levelrelative <~ ")") ~ "to" ~ (levelsingle ||| "(" ~> levelrelative <~ ")")
    ^^ { case b ~ _ ~ e => L2_LevelRange(b, e) })

  lazy val levelrelative = locationize(levelsingle ~ ("+" ||| "-") ~ integerLit
    ^^ { case l ~ op ~ i => L2_RelativeLevel(l, op, i) })

  lazy val levelall = locationize("all" ^^ { _ => L2_AllLevels })

  lazy val levelsingle = (
    locationize("current" ^^ { _ => L2_CurrentLevel })
      ||| locationize("coarser" ^^ { _ => L2_CoarserLevel })
      ||| locationize("finer" ^^ { _ => L2_FinerLevel })
      ||| locationize("coarsest" ^^ { _ => L2_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L2_FinestLevel })
      ||| locationize(integerLit ^^ { L2_SingleLevel }))

  // #############################################################################
  // ################################## BASE_EXT #################################
  // #############################################################################

  // ######################################
  // ##### L2_HigherOrderDatatype
  // ######################################

  lazy val higherOrderDatatype : Parser[L2_Datatype] = (("Array" ||| "array") ~> ("<" ~> datatype <~ ">") ~ ("<" ~> integerLit <~ ">")
    ^^ { case x ~ s => L2_ArrayDatatype(x, s) })

  // ######################################
  // ##### L2_UnresolvedAccess
  // ######################################

  lazy val genericAccess = locationize(ident ~ levelAccess.? ^^ { case id ~ level => L2_UnresolvedAccess(id, level) })

  // #############################################################################
  // ################################## BOUNDARY #################################
  // #############################################################################

  lazy val fieldBoundary = (
    "Neumann" ~> ("(" ~> integerLit <~ ")").? ^^ { L2_NeumannBC(_) }
      ||| "None" ^^ { _ => L2_NoBC }
      ||| binaryexpression ^^ { L2_DirichletBC }
    )

  // #############################################################################
  // ################################### DOMAIN ##################################
  // #############################################################################

  // ######################################
  // ##### L2_DomainDecl
  // ######################################

  lazy val domainDeclaration = locationize("Domain" ~> ident ^^ { L2_DomainDecl })

  // #############################################################################
  // #################################### FIELD ##################################
  // #############################################################################

  // ######################################
  // ##### l2_FieldDeclarations
  // ######################################

  lazy val fieldDeclaration = baseFieldDeclaration ||| boundaryFieldDeclaration

  lazy val localization = ("Node" ||| "node" ||| "Cell" ||| "cell"
    ||| "Face_x" ||| "face_x" ||| "Face_y" ||| "face_y" ||| "Face_z" ||| "face_z"
    ||| "Edge_Node" ||| "edge_node" ||| "Edge_Cell" ||| "edge_cell"
    ^^ (l => l))

  lazy val baseFieldDeclaration = locationize(("Field" ~> ident) ~ level.? ~ ("with" ~> datatype).? ~ ("on" ~> localization) ~ ("of" ~> ident) ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ datatype ~ localization ~ domain ~ initial => L2_BaseFieldDecl(id, levels, datatype, localization, domain, initial) })
  lazy val boundaryFieldDeclaration = locationize(("Field" ~> ident) ~ level.? ~ ("on" ~> "boundary") ~ ("=" ~> fieldBoundary)
    ^^ { case id ~ levels ~ _ ~ bc => L2_BoundaryFieldDecl(id, levels, bc) })

  // #############################################################################
  // ################################## OPERATOR #################################
  // #############################################################################

  //  // ######################################
  //  // ##### L2_OperatorDecl
  //  // ######################################
  //
  //  lazy val operatorDeclaration = (
  //    locationize(("Operator" ~> ident) ~ ("from" ~> stencilDeclaration) ^^ { case id ~ stencil => stencil.name = id; L2_OperatorFromStencil(id, stencil) })
  //      ||| locationize(("Operator" ~> ident) ~ ("from" ~> stencilTemplateDeclaration) ^^ { case id ~ stencil => stencil.name = id; L2_OperatorFromStencilTemplate(id, stencil) }))

  // ######################################
  // ##### L2_StencilDecl
  // ######################################

  lazy val stencilDeclaration = locationize(("Operator" ~> ident <~ ("from" ~ "Stencil")) ~ ("{" ~> stencilEntries <~ "}")
    ^^ { case id ~ entries => L2_StencilDecl(id, entries) })
  lazy val stencilEntries = (
    (stencilEntry <~ ",").+ ~ stencilEntry ^^ { case entries ~ entry => entries.::(entry) }
      ||| stencilEntry.+)
  lazy val stencilEntry = locationize((index ~ ("=>" ~> binaryexpression)) ^^ { case offset ~ coeff => L2_StencilEntry(offset, coeff) })

  // ######################################
  // ##### L2_StencilTemplateDecl
  // ######################################

  lazy val stencilTemplateDeclaration = locationize(("Operator" ~> ident) ~ (("from" ~ "StencilTemplate" ~ "on") ~> localization) ~ ("of" ~> ident) ~ ("{" ~> stencilTemplateEntries <~ "}")
    ^^ { case id ~ local ~ domain ~ offsets => L2_StencilTemplateDecl(id, local, domain, offsets) })
  lazy val stencilTemplateEntries = (
    (stencilTemplateEntry <~ ",").+ ~ stencilTemplateEntry ^^ { case entries ~ entry => entries.::(entry) }
      ||| stencilTemplateEntry.+)
  lazy val stencilTemplateEntry = locationize((index <~ "=>") ^^ { offset => offset })

}
