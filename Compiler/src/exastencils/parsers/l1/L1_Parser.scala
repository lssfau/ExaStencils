package exastencils.parsers.l1

import scala.collection.immutable.PagedSeq
import scala.collection.mutable._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import exastencils.base.l1._
import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.discretization.l1._
import exastencils.domain.l1._
import exastencils.field.l1.L1_BaseFieldDecl
import exastencils.parsers._

object L1_Parser extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = L1_Lexer

  def parse(s : String) : L1_Root = {
    parseTokens(new lexical.Scanner(s))
  }

  private val prevDirs = new Stack[java.io.File]().push(null)
  def parseFile(filename : String) : L1_Root = {
    val file = new java.io.File(prevDirs.top, filename)
    val lines = scala.io.Source.fromFile(file).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    prevDirs.push(file.getAbsoluteFile.getParentFile)
    val ret = parseTokens(scanner)
    prevDirs.pop()
    ret.asInstanceOf[L1_Root]
  }

  protected def parseTokens(tokens : lexical.Scanner) : L1_Root = {
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

  /** implicit conversion for (latex, unicode)string-pairs */
  implicit def stringPairToParser(pair : (String, String)) = pair._1 | pair._2

  //###########################################################

  lazy val program = (
    import_
      ||| domainDeclaration
      ||| fieldDeclaration
      ||| discretizeBlock
    ).* ^^ { L1_Root(_) }

  lazy val import_ = "import" ~> stringLit ^^ { parseFile }

  //###########################################################

  // #############################################################################
  // #################################### BASE ###################################
  // #############################################################################

  // ######################################
  // ##### L1_BinaryOps
  // ######################################

  lazy val binaryexpression : PackratParser[L1_Expression] = (
    locationize((binaryexpression ~ ("+" ||| "-" ||| ".+" ||| ".-") ~ term) ^^ { case lhs ~ op ~ rhs => L1_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term)

  lazy val term : PackratParser[L1_Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "%" ||| ".*" ||| "./" ||| ".%") ~ term2) ^^ { case lhs ~ op ~ rhs => L1_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term2)

  lazy val term2 : PackratParser[L1_Expression] = (
    locationize((term2 ~ ("**" ||| "^" ||| ".**") ~ factor) ^^ { case lhs ~ op ~ rhs => L1_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| factor)

  lazy val factor = (
    "(" ~> binaryexpression <~ ")"
      ||| ("-" ~ "(") ~> binaryexpression <~ ")" ^^ { exp => L1_UnaryOperators.createExpression("-", exp) }
      ||| locationize(stringLit ^^ { s => L1_StringConstant(s) })
      ||| locationize("-".? ~ numericLit ^^ { case s ~ n =>
      if (isInt(s.getOrElse("") + n)) L1_IntegerConstant((s.getOrElse("") + n).toInt)
      else L1_RealConstant((s.getOrElse("") + n).toDouble)
    })
      ||| locationize("-" ~> functionCall ^^ { x => L1_UnaryOperators.createExpression("-", x) })
      ||| functionCall
      ||| locationize("-" ~> genericAccess ^^ { x => L1_UnaryOperators.createExpression("-", x) })
      ||| genericAccess
      ||| locationize(booleanLit ^^ { s => L1_BooleanConstant(s) }))

  lazy val booleanexpression : PackratParser[L1_Expression] = (
    locationize((booleanexpression ~ ("||" ||| "or") ~ booleanexpression1) ^^ { case ex1 ~ op ~ ex2 => L1_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression1)

  lazy val booleanexpression1 : PackratParser[L1_Expression] = (
    locationize((booleanexpression1 ~ ("&&" ||| "and") ~ booleanexpression2) ^^ { case ex1 ~ op ~ ex2 => L1_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression2)

  lazy val booleanexpression2 : PackratParser[L1_Expression] = (
    locationize(("!" ~> booleanexpression3) ^^ { ex => L1_UnaryOperators.createExpression("!", ex) })
      ||| booleanexpression3)

  lazy val booleanexpression3 : PackratParser[L1_Expression] = (
    "(" ~> booleanexpression <~ ")"
      ||| comparison
      ||| binaryexpression)

  lazy val comparison : PackratParser[L1_Expression] = //(
    locationize((binaryexpression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ binaryexpression) ^^ { case ex1 ~ op ~ ex2 => L1_BinaryOperators.createExpression(op, ex1, ex2) })

  // ######################################
  // ##### L1_Function
  // ######################################

  lazy val functionReference = locationize(ident ~ levelAccess.?
    ^^ { case id ~ level => L1_UnresolvedFunctionReference(id, level) })

  lazy val functionCallArgumentList = /*locationize*/ ((binaryexpression ||| booleanexpression) <~ ("," | newline)).* ~ (binaryexpression ||| booleanexpression) ^^ { case exps ~ ex => exps :+ ex }
  lazy val functionCall = locationize(functionReference ~ ("(" ~> functionCallArgumentList.? <~ ")") ^^ { case id ~ args => L1_FunctionCall(id, args) })

  // ######################################
  // ##### L1_Interval
  // ######################################

  lazy val interval = locationize(("(" ~> realLit <~ ",") ~ (realLit <~ ")")
    ^^ { case begin ~ end => L1_Interval(begin, end) })

  // ######################################
  // ##### L1_LevelSpecification
  // ######################################

  lazy val levelDecl = locationize("@" ~> (
    directDeclLevel ||| ("(" ~> relativeDeclLevel <~ ")")
      ||| allLevels ||| ("(" ~> (levelDeclRange ||| levelDeclList ||| levelDeclNegList) <~ ")")) ^^ { l => l })

  lazy val levelAccess = locationize("@" ~> (directAccessLevel ||| ("(" ~> relativeAccessLevel <~ ")")))

  lazy val levelDeclGroup = (
    levelDeclRange
      ||| levelDeclList
      ||| allLevels
      ||| singleDeclLevel
    )

  lazy val allLevels = locationize("all" ^^ { _ => L1_AllLevels })

  lazy val levelDeclRange = locationize((singleDeclLevel <~ "to") ~ singleDeclLevel ^^ { case b ~ e => L1_LevelRange(b, e) })

  lazy val levelDeclList : Parser[L1_DeclarationLevelSpecification] = (
    locationize((singleDeclLevel <~ ("," ||| "and")).+ ~ singleDeclLevel ^^ { case a ~ b => L1_LevelList(a :+ b) })
      ||| locationize("(" ~> levelDeclList <~ ")") ^^ { l => l })

  lazy val levelDeclNegList : Parser[L1_LevelList] = (
    locationize((levelDeclGroup <~ ("but" ||| "not")) ~ levelDeclGroup ^^ { case in ~ out => L1_LevelList(List(in, L1_NegatedLevelList(out))) })
      ||| locationize("(" ~> levelDeclNegList <~ ")") ^^ { l => l })

  lazy val singleAccessLevel : Parser[L1_AccessLevelSpecification] = (
    directAccessLevel
      ||| relativeAccessLevel
      ||| locationize("(" ~> singleAccessLevel <~ ")") ^^ { l => l })

  lazy val singleDeclLevel : Parser[L1_DeclarationLevelSpecification] = (
    directDeclLevel
      ||| relativeDeclLevel
      ||| locationize("(" ~> singleDeclLevel <~ ")") ^^ { l => l })

  lazy val relativeAccessLevel = locationize(directAccessLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L1_RelativeLevel(l, op, i) })
  lazy val relativeDeclLevel = locationize(directDeclLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L1_RelativeLevel(l, op, i) })

  lazy val directAccessLevel : Parser[L1_AccessLevelSpecification] = (
    locationize("current" ^^ { _ => L1_CurrentLevel })
      ||| locationize("coarser" ^^ { _ => L1_CoarserLevel })
      ||| locationize("finer" ^^ { _ => L1_FinerLevel })
      ||| locationize("coarsest" ^^ { _ => L1_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L1_FinestLevel })
      ||| locationize(integerLit ^^ { l => L1_SingleLevel(l) })
      ||| locationize("(" ~> directAccessLevel <~ ")" ^^ { l => l }))

  lazy val directDeclLevel : Parser[L1_DeclarationLevelSpecification] = (
    locationize("coarsest" ^^ { _ => L1_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L1_FinestLevel })
      ||| locationize(integerLit ^^ { l => L1_SingleLevel(l) })
      ||| locationize("(" ~> directDeclLevel <~ ")" ^^ { l => l }))

  // ######################################
  // ##### L1_UnresolvedAccess
  // ######################################

  lazy val genericAccess = locationize(ident ~ levelAccess.? ^^ { case id ~ level => L1_UnresolvedAccess(id, level) })

  // #############################################################################
  // ############################### DISCRETIZATION ##############################
  // #############################################################################

  // ######################################
  // ##### L1_DiscretizationStatement
  // ######################################

  lazy val discretizationStmt = fieldDiscr // ||| ...

  lazy val fieldDiscr = locationize(ident ~ levelDecl.? ~ ("=>" ~> ident).? ~ ("on" ~> localization)
    ^^ { case src ~ levels ~ map ~ local => L1_FieldDiscretization(src, levels, map, local) })

  // ######################################
  // ##### L1_DiscretizeBlock
  // ######################################

  lazy val discretizeBlock = locationize(("Discretize" ~ "{") ~> discretizationStmt.* <~ "}"
    ^^ (L1_DiscretizeBlock(_)))

  // #############################################################################
  // ################################### DOMAIN ##################################
  // #############################################################################

  // ######################################
  // ##### L1_DomainDecl
  // ######################################

  lazy val realIndex = /*locationize*/ "[" ~> realLit ~ ("," ~> realLit).* <~ "]" ^^ { case b ~ l => (List(b) ++ l).toArray }
  lazy val domainDeclaration = (locationize(("Domain" ~> ident <~ "=") ~ interval ~ (L1_ReservedSigns.times ~> interval).*
    ^^ { case id ~ head ~ tail => L1_DomainFromIntervalsDecl(id, List(head) ++ tail) })
    ||| locationize(("Domain" ~> ident <~ "=") ~ (realIndex <~ "to") ~ realIndex ^^ { case id ~ l ~ u => L1_DomainFromAABBDecl(id, l, u) })
    )

  // #############################################################################
  // #################################### FIELD ##################################
  // #############################################################################

  // ######################################
  // ##### L1_FieldDeclarations
  // ######################################

  lazy val fieldDeclaration = locationize(("Field" ~> ident) ~ levelDecl.? ~ (L1_ReservedSigns.elemOf ~> ident) ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ domain ~ initial => L1_BaseFieldDecl(id, levels, domain, initial) })

  // #############################################################################
  // #################################### GRID ###################################
  // #############################################################################

  // ######################################
  // ##### L1_Localization
  // ######################################

  lazy val localization = ("Node" ||| "node" ||| "Cell" ||| "cell"
    ||| "Face_x" ||| "face_x" ||| "Face_y" ||| "face_y" ||| "Face_z" ||| "face_z"
    ||| "Edge_Node" ||| "edge_node" ||| "Edge_Cell" ||| "edge_cell"
    ^^ (l => l))
}
