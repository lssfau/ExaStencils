package exastencils.parsers.l1

import scala.collection.immutable.PagedSeq
import scala.collection.mutable._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import exastencils.base.l1._
import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.boundary.l1._
import exastencils.discretization.l1._
import exastencils.domain.l1._
import exastencils.field.l1._
import exastencils.operator.l1._
import exastencils.parsers._
import exastencils.solver.l1._

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
      ||| operatorDeclaration
      ||| equationDeclaration
      ||| discretizationHints
      ||| solverHints
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
      ||| locationize("-" ~> partialDerivative ^^ { x => L1_UnaryOperators.createExpression("-", x) })
      ||| partialDerivative
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
  // ################################## BOUNDARY #################################
  // #############################################################################

  lazy val fieldBoundary = (
    "Neumann" ~> ("(" ~> integerLit <~ ")").? ^^ { L1_NeumannBC(_) }
      ||| "None" ^^ { _ => L1_NoBC }
      ||| binaryexpression ^^ { L1_DirichletBC }
    )

  // #############################################################################
  // ############################### DISCRETIZATION ##############################
  // #############################################################################

  // ######################################
  // ##### L1_DiscretizationHints
  // ######################################

  lazy val discretizationHint = fieldDiscr ||| operatorDiscr ||| equationDiscr ||| discretizationParameter

  lazy val fieldDiscr = locationize(ident ~ levelDecl.? ~ ("=>" ~> ident).? ~ ("on" ~> localization)
    ^^ { case src ~ levels ~ map ~ local => L1_FieldDiscretization(src, levels, map, local) })

  lazy val operatorDiscr = locationize(ident ~ levelDecl.? ~ ("=>" ~> ident).? ~ ("with" ~> stringLit) ~ ("on" ~> ident)
    ^^ { case src ~ levels ~ map ~ discr ~ domain => L1_OperatorDiscretization(src, levels, map, discr, domain) })

  lazy val equationDiscr = locationize(ident ~ levelDecl.? ~ ("=>" ~> ident).?
    ^^ { case src ~ levels ~ map => L1_EquationDiscretization(src, levels, map) })

  lazy val discretizationParameter = locationize((ident <~ "=") ~ literal ^^ { case param ~ value => L1_DiscretizationParameter(param, value) })

  lazy val discretizationHints = locationize((("Discretize" ||| "DiscretizationHint" ||| "L2Hint") ~ "{") ~> discretizationHint.* <~ "}"
    ^^ (L1_DiscretizationHints(_)))

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

  lazy val fieldDeclaration = baseFieldDeclaration ||| boundaryFieldDeclaration

  lazy val baseFieldDeclaration = locationize(("Field" ~> ident) ~ levelDecl.? ~ (L1_ReservedSigns.elemOf ~> ident) ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ domain ~ initial => L1_BaseFieldDecl(id, levels, domain, initial) })
  lazy val boundaryFieldDeclaration = locationize(("Field" ~> ident) ~ levelDecl.? ~ ((L1_ReservedSigns.elemOf ~ L1_ReservedSigns.partial) ~> ident) ~ ("=" ~> fieldBoundary)
    ^^ { case id ~ levels ~ domain ~ bc => L1_BoundaryFieldDecl(id, levels, domain, bc) })

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

  // #############################################################################
  // ################################## OPERATOR #################################
  // #############################################################################

  // ######################################
  // ##### L1_OperatorDecl
  // ######################################

  lazy val operatorDeclaration = locationize(("Operator" ~> ident) ~ levelDecl.? ~ ("=" ~> binaryexpression)
    ^^ { case id ~ levels ~ expr => L1_OperatorDecl(id, levels, expr) })

  // ######################################
  // ##### L1_PartialDerivative
  // ######################################

  lazy val partialDerivative = (
    locationize((L1_ReservedSigns.partial ~ "_".? ~ "{") ~> ident <~ "}" ^^ (L1_PartialDerivative(_)))
      ||| locationize(L1_ReservedSigns.capitalDelta ^^ { _ => L1_Laplace }))

  // #############################################################################
  // ################################### SOLVER ##################################
  // #############################################################################

  // ######################################
  // ##### L1_Equation
  // ######################################

  lazy val equation = locationize((binaryexpression <~ ("=" | "==")) ~ binaryexpression ^^ { case lhs ~ rhs => L1_Equation(lhs, rhs) })

  // ######################################
  // ##### L1_EquationDecl
  // ######################################

  lazy val equationDeclaration = locationize(("Equation" ~> ident) ~ levelDecl.? ~ equation
    ^^ { case id ~ levels ~ eq => L1_EquationDecl(id, levels, eq) })

  // ######################################
  // ##### L1_SolverForEquation
  // ######################################

  lazy val solverForEqEntry = locationize((ident <~ "in") ~ ident ^^ { case unknownName ~ eqName => L1_SolverForEqEntry(unknownName, eqName) })
  lazy val solverForEq = locationize(("generate" ~ "solver" ~ "for") ~> (solverForEqEntry <~ "and").* ~ solverForEqEntry
    ^^ { case entries ~ tail => L1_SolverForEquation(entries :+ tail) })

  // ######################################
  // ##### L1_SolverHints
  // ######################################

  lazy val solverHint = solverForEq ||| solverParameter

  lazy val solverParameter = locationize((ident <~ "=") ~ literal ^^ { case param ~ value => L1_SolverParameter(param, value) })

  lazy val solverHints = locationize((("solve" ||| "SolverHint" ||| "L3Hint") ~ "{") ~> solverHint.* <~ "}"
    ^^ (L1_SolverHints(_)))
}
