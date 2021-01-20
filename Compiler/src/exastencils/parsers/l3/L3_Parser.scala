//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.parsers.l3

import scala.collection.mutable._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import exastencils.base.l3._
import exastencils.baseExt.l3.ComplexNumbers
import exastencils.baseExt.l3.ComplexNumbers.L3_ComplexExpression
import exastencils.baseExt.l3._
import exastencils.boundary.l3._
import exastencils.domain.l3.L3_DomainFromAABBDecl
import exastencils.field.l3._
import exastencils.knowledge.l3._
import exastencils.logger.Logger
import exastencils.operator.l3._
import exastencils.parsers._
import exastencils.solver.l3._
import exastencils.util.l3.L3_OffsetAlias

object L3_Parser extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = L3_Lexer

  def parse(s : String, filename : String = "") : L3_Root = {
    filenames.push(filename)
    val ret = parseTokens(new lexical.Scanner(s))
    filenames.pop()
    ret.asInstanceOf[L3_Root]
  }

  private val prevDirs = new Stack[java.io.File]().push(null)
  def parseFile(filename : String) : L3_Root = {
    val file = new java.io.File(prevDirs.top, filename)
    val lines = scala.io.Source.fromFile(file).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))

    prevDirs.push(file.getAbsoluteFile.getParentFile)
    val ret = parse(reader.source.toString, file.getName)
    prevDirs.pop()
    ret.asInstanceOf[L3_Root]
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

  lazy val program = (
    import_
      ||| inlineKnowledge
      ||| domainDeclaration
      ||| fieldDeclaration
      ||| overrideFieldInformation
      ||| fieldCombinationDeclaration
      ||| stencilDeclaration
      ||| stencilTemplateDeclaration
      ||| stencilFromDefault
      ||| equationDeclaration
      ||| globals
      ||| function
      ||| functionTemplate
      ||| functionInstantiation
      ||| solverForEq
      ||| applicationHints
    ).* ^^ { L3_Root(_) }

  lazy val import_ = "import" ~> stringLit ^^ { parseFile }

  //###########################################################

  // #############################################################################
  // #################################### BASE ###################################
  // #############################################################################

  // ######################################
  // ##### L3_Assignment
  // ######################################

  lazy val generalAssignment = locationize(genericAccess ~ ("=" ||| "+=" ||| "-=" ||| "*=" ||| "/=") ~ (binaryexpression ||| booleanexpression) ~ ("where" ~> booleanexpression).?
    ^^ { case id ~ op ~ exp ~ cond => L3_Assignment(id, exp, op, cond) })

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
      ||| fieldIteratorAccess
      ||| locationize(booleanLit ^^ { L3_BooleanConstant })
      ||| complexExpression
    )

  lazy val complexExpression : PackratParser[L3_Expression] = locationize(
    ("(" ~> term) ~ "+" ~ term <~ ("j" ~ ")")
  ^^ { case  real ~ _ ~ imag => L3_ComplexExpression(real, true, imag)} |||
  ("(" ~> term) ~ "-" ~ term <~ ("j" ~ ")")
    ^^ { case  real ~ _ ~ imag => ComplexNumbers.L3_ComplexExpression(real, false, imag)} |||
  ("complex" ~ "(") ~> term ~ "," ~ "-".? ~ (term <~ ")")
    ^^ { case  real ~ _ ~ sign ~ imag => ComplexNumbers.L3_ComplexExpression(real, if(sign.isDefined) false else true, imag)})

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
  // ##### L3_Conditional
  // ######################################

  lazy val conditional : PackratParser[L3_IfCondition] = (
    locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ~ (("else" ~ "{") ~> statement.+ <~ "}").?
      ^^ { case exp ~ stmts ~ elseStmts => L3_IfCondition(exp, stmts.to[ListBuffer], elseStmts.getOrElse(ListBuffer()).to[ListBuffer]) })
      ||| locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ~ ("else" ~> conditional)
      ^^ { case exp ~ stmts ~ elseCond => L3_IfCondition(exp, stmts.to[ListBuffer], ListBuffer[L3_Statement](elseCond)) }))

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
      ||| ("Float" ||| "float" ||| "Flt") ^^ { _ => L3_FloatDatatype }
      ||| ("Double" ||| "double" ||| "Dbl") ^^ { _ => L3_DoubleDatatype })

  lazy val returnDatatype = (("Unit" ||| "unit") ^^ { _ => L3_UnitDatatype }
    ||| datatype)

  // ######################################
  // ##### L3_Declaration
  // ######################################

  lazy val localDeclaration = variableDeclaration ||| valueDeclaration

  lazy val variableDeclaration = locationize((("Var" ||| "Variable") ~> ident) ~ levelDecl.? ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ dt ~ exp => L3_VariableDeclaration(id, levels, dt, exp, false) })

  lazy val valueDeclaration = locationize((("Val" ||| "Value") ~> ident) ~ levelDecl.? ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression))
    ^^ { case id ~ levels ~ dt ~ exp => L3_VariableDeclaration(id, levels, dt, Some(exp), true) })

  // ######################################
  // ##### L3_FunctionDecl
  // ######################################

  lazy val function = locationize((("Func" ||| "Function") ~> ident) ~ levelDecl.? ~ ("(" ~> functionArgumentList.? <~ ")").? ~ (":" ~> returnDatatype).? ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case name ~ levels ~ args ~ returnType ~ stmts => L3_FunctionDecl(name, levels, returnType, args, stmts) })
  lazy val functionArgumentList = /*locationize*/ repsep(functionArgument, listdelimiter)
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => L3_Function.Argument(id, t) })

  lazy val returnStatement = locationize("return" ~> (binaryexpression ||| booleanexpression).? ^^ { L3_Return })

  lazy val functionReference = locationize(ident ~ levelAccess.? ~ ("@" ~> constIndex).? ^^ { case id ~ level ~ offset => L3_UnresolvedFunctionReference(id, level, offset) })
  lazy val functionCallArgumentList = /*locationize*/ repsep(binaryexpression ||| booleanexpression, listdelimiter)
  lazy val functionCall = locationize(functionReference ~ ("(" ~> functionCallArgumentList <~ ")") ^^ { case id ~ args => L3_FunctionCall(id, args : _*) })

  // ######################################
  // ##### L3_Index
  // ######################################

  lazy val index = expressionIndex ||| constIndex

  lazy val expressionIndex = locationize("[" ~> repsep(binaryexpression, ",") <~ "]" ^^ (l => L3_ExpressionIndex(l.toArray)))
  lazy val constIndex = (
    locationize("[" ~> repsep(integerLit, ",") <~ "]" ^^ (l => L3_ConstIndex(l.toArray)))
      ||| ("center" ||| "east" ||| "west" ||| "north" ||| "south" ||| "top" ||| "bottom") ^^ (s => L3_OffsetAlias.toConstIndex(s)))

  // ######################################
  // ##### L3_LevelSpecification
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

  lazy val allLevels = locationize("all" ^^ { _ => L3_AllLevels })

  lazy val levelDeclRange = locationize((singleDeclLevel <~ "to") ~ singleDeclLevel ^^ { case b ~ e => L3_LevelRange(b, e) })

  lazy val levelDeclList : Parser[L3_DeclarationLevelSpecification] = (
    locationize((singleDeclLevel <~ ("," ||| "and")).+ ~ singleDeclLevel ^^ { case a ~ b => L3_LevelList(a :+ b) })
      ||| locationize("(" ~> levelDeclList <~ ")") ^^ { l => l })

  lazy val levelDeclNegList : Parser[L3_LevelList] = (
    locationize((levelDeclGroup <~ ("but" ||| "not")) ~ levelDeclGroup ^^ { case in ~ out => L3_LevelList(List(in, L3_NegatedLevelList(out))) })
      ||| locationize("(" ~> levelDeclNegList <~ ")") ^^ { l => l })

  lazy val singleAccessLevel : Parser[L3_AccessLevelSpecification] = (
    directAccessLevel
      ||| relativeAccessLevel
      ||| locationize("(" ~> singleAccessLevel <~ ")") ^^ { l => l })

  lazy val singleDeclLevel : Parser[L3_DeclarationLevelSpecification] = (
    directDeclLevel
      ||| relativeDeclLevel
      ||| locationize("(" ~> singleDeclLevel <~ ")") ^^ { l => l })

  lazy val relativeAccessLevel = locationize(directAccessLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L3_RelativeLevel(l, op, i) })
  lazy val relativeDeclLevel = locationize(directDeclLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L3_RelativeLevel(l, op, i) })

  lazy val directAccessLevel : Parser[L3_AccessLevelSpecification] = (
    locationize("current" ^^ { _ => L3_CurrentLevel })
      ||| locationize("coarser" ^^ { _ => L3_CoarserLevel })
      ||| locationize("finer" ^^ { _ => L3_FinerLevel })
      ||| locationize("coarsest" ^^ { _ => L3_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L3_FinestLevel })
      ||| locationize(integerLit ^^ { l => L3_SingleLevel(l) })
      ||| locationize("(" ~> directAccessLevel <~ ")" ^^ { l => l }))

  lazy val directDeclLevel : Parser[L3_DeclarationLevelSpecification] = (
    locationize("coarsest" ^^ { _ => L3_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L3_FinestLevel })
      ||| locationize(integerLit ^^ { l => L3_SingleLevel(l) })
      ||| locationize("(" ~> directDeclLevel <~ ")" ^^ { l => l }))

  // ######################################
  // ##### L3_Loop
  // ######################################

  lazy val countLoop = locationize(("repeat" ~> numericLit <~ "times") ~ ("count" ~> genericAccess).? ~ ("{" ~> statement.+ <~ "}")
    ^^ { case numIt ~ it ~ stmts => L3_ForLoop(numIt.toInt, it, stmts.to[ListBuffer]) })

  lazy val untilLoop = locationize((("repeat" ~ "until") ~> booleanexpression) ~ (("{" ~> statement.+) <~ "}")
    ^^ { case cond ~ stmts => L3_UntilLoop(cond, stmts.to[ListBuffer]) })

  lazy val whileLoop = locationize((("repeat" ~ "while") ~> booleanexpression) ~ (("{" ~> statement.+) <~ "}")
    ^^ { case cond ~ stmts => L3_WhileLoop(cond, stmts.to[ListBuffer]) })

  // ######################################
  // ##### L3_Statement
  // ######################################

  lazy val statement : Parser[L3_Statement] = (
    localDeclaration
      ||| expressionDeclaration
      ||| generalAssignment
      ||| conditional
      ||| countLoop
      ||| untilLoop
      ||| whileLoop
      ||| functionCall ^^ { L3_ExpressionStatement(_) }
      ||| returnStatement
      ||| levelScope
      ||| solveLocallyStatement
      ||| colorWithStatement
      ||| repeatWithStatement)

  // #############################################################################
  // ################################## BASE_EXT #################################
  // #############################################################################

  // ######################################
  // ##### L3_ApplicationHints
  // ######################################

  lazy val applicationHint = applicationParameter
  lazy val applicationParameter = locationize((ident <~ "=") ~ literal ^^ { case param ~ value => L3_ApplicationParameter(param, value) })
  lazy val applicationHints = locationize((("ApplicationHint" ||| "ApplicationHints" ||| "L4Hint" ||| "L4Hints") ~ "{") ~> applicationHint.* <~ "}"
    ^^ (L3_ApplicationHints(_)))

  // ######################################
  // ##### L3_ColorLoops
  // ######################################

  lazy val colorWithStatement = locationize(("color" ~ "with" ~ "{") ~> (colorDefExpression <~ ",").+ ~ statement.* <~ "}"
    ^^ { case colors ~ stmts => L3_ColorLoops(colors.to, stmts.to) })
  lazy val colorDefExpression : Parser[L3_Modulo] = locationize(binaryexpression ^^ {
    case modExp @ L3_Modulo(_, L3_IntegerConstant(_)) => modExp
    case exp                                          => Logger.error("color expression in 'color with' statement must be a modulo expression with a constant integral divisor" + exp.location.toAppendString)
  })

  // ######################################
  // ##### L3_ExpressionDeclaration
  // ######################################

  //TODO parse complex expression
  lazy val expressionDeclaration = locationize((("Expr" ||| "Expression") ~> ident) ~ levelDecl.? ~ ("=" ~> (binaryexpression ||| booleanexpression
    //||| complexExpression
    ))
    ^^ { case id ~ levels ~ exp => L3_ExpressionDeclaration(id, levels, exp) })

  // ######################################
  // ##### L3_FieldIteratorAccess
  // ######################################

  lazy val fieldIteratorAccess = (
    locationize(("i0" | "i1" | "i2") ^^ { id => L3_FieldIteratorAccess(id) })
      ||| locationize(("x" | "y" | "z") ^^ { id => L3_FieldIteratorAccess(id) })
    )

  // ######################################
  // ##### l3_FunctionTemplate
  // ######################################

  lazy val functionTemplateArgList = /*locationize*/ repsep(ident, listdelimiter)
  lazy val functionTemplate = locationize((("FuncTemplate" ||| "FunctionTemplate") ~> ident) ~ ("<" ~> functionTemplateArgList <~ ">") ~ ("(" ~> functionArgumentList <~ ")").? ~ (":" ~> returnDatatype).? ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case id ~ templateArgs ~ functionArgs ~ retType ~ stmts => L3_FunctionTemplate(id, retType, templateArgs, functionArgs, stmts) })

  // ######################################
  // ##### l3_FunctionInstantiation
  // ######################################

  lazy val functionInstArgList = /*locationize*/ repsep(functionInstArgument, listdelimiter)
  lazy val functionInstArgument = binaryexpression ||| booleanexpression
  lazy val functionInstantiation = locationize(((("Inst" ||| "Instantiate") ~> ident) ~ ("<" ~> functionInstArgList.? <~ ">") ~ ("as" ~> ident) ~ levelDecl.?)
    ^^ { case template ~ args ~ target ~ targetLevel => L3_FunctionInstantiation(template, args.getOrElse(List()), target, targetLevel) })

  // ######################################
  // ##### L3_GlobalSection
  // ######################################

  lazy val globals = locationize(("Globals" ~> "{" ~> globalEntry.* <~ "}") ^^ { L3_GlobalSection(_) })
  lazy val globalEntry = locationize(valueDeclaration ||| variableDeclaration ||| expressionDeclaration)

  // ######################################
  // ##### L3_HigherOrderDatatype
  // ######################################

  lazy val higherOrderDatatype : Parser[L3_Datatype] = (
    "Vector" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L3_VectorDatatype(x, s) }
      ||| ("RowVector" ||| "RVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L3_VectorDatatype(x, s, true) }
      ||| ("ColumnVector" ||| "CVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L3_VectorDatatype(x, s, false) }
      ||| numericDatatype ~ ("<" ~> integerLit <~ ">") ^^ { case x ~ s => L3_VectorDatatype(x, s) }
      ||| "Vec2" ^^ { _ => L3_VectorDatatype(L3_RealDatatype, 2) }
      ||| "Vec3" ^^ { _ => L3_VectorDatatype(L3_RealDatatype, 3) }
      ||| "Vec4" ^^ { _ => L3_VectorDatatype(L3_RealDatatype, 4) }
      ||| "Matrix" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ m ~ n => L3_MatrixDatatype(x, m, n) }
      ||| numericDatatype ~ ("<" ~> integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case x ~ m ~ n => L3_MatrixDatatype(x, m, n) })

  // ######################################
  // ##### L3_LevelScope
  // ######################################

  lazy val levelScope = locationize(((levelDecl ||| levelAccess) <~ "{") ~ (statement.+ <~ "}") ^^ { case l ~ s => L3_LevelScope(l, s) })

  // ######################################
  // ##### L3_RepeatLoops
  // ######################################

  lazy val repeatWithStatement = locationize(("repeat" ~ "with" ~ "{") ~> (booleanexpression <~ ",").+ ~ statement.* <~ "}"
    ^^ { case conds ~ stmts => L3_RepeatLoops(conds, stmts) })

  // ######################################
  // ##### L3_UnresolvedAccess
  // ######################################

  lazy val genericAccess = locationize(ident ~ levelAccess.? ~ ("@" ~> constIndex).? ~ (":" ~> constIndex).?
    ^^ { case id ~ level ~ offset ~ dirAccess => L3_UnresolvedAccess(id, level, None, offset, dirAccess, None) })

  // #############################################################################
  // ################################## BOUNDARY #################################
  // #############################################################################

  lazy val fieldBoundary = (
    "Neumann" ~> ("(" ~> integerLit <~ ")").? ^^ { L3_NeumannBC(_) }
      ||| "None" ^^ { _ => L3_NoBC }
      ||| binaryexpression ^^ { L3_DirichletBC }
    )

  // #############################################################################
  // ################################### DOMAIN ##################################
  // #############################################################################

  // ######################################
  // ##### L3_DomainDecl
  // ######################################

  lazy val realIndex = /*locationize*/ "[" ~> realLit ~ ("," ~> realLit).* <~ "]" ^^ { case b ~ l => (List(b) ++ l).toArray }
  lazy val domainDeclaration = (
    locationize(("Domain".? ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l ~ u => L3_DomainFromAABBDecl(id, l, u) })
      ||| locationize(("Domain".? ~> ident) ~ (" from " ~> realIndex <~ "to") ~ realIndex ^^ { case id ~ l ~ u => L3_DomainFromAABBDecl(id, l, u) }))

  // #############################################################################
  // #################################### FIELD ##################################
  // #############################################################################

  // ######################################
  // ##### L3_FieldDecl
  // ######################################

  lazy val fieldDeclaration = baseFieldDeclaration ||| boundaryFieldDeclaration ||| fieldFromOther

  lazy val localization = ("Node" ||| "node" ||| "Cell" ||| "cell"
    ||| "Face_x" ||| "face_x" ||| "Face_y" ||| "face_y" ||| "Face_z" ||| "face_z"
    ||| "Edge_Node" ||| "edge_node" ||| "Edge_Cell" ||| "edge_cell"
    ^^ (l => l))

  lazy val baseFieldDeclaration = locationize(("Field" ~> ident) ~ levelDecl.? ~ ("with" ~> datatype).? ~ ("on" ~> localization) ~ ("of" ~> ident) ~ (integerLit <~ "times").? ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ datatype ~ localization ~ domain ~ numSlots ~ initial => L3_BaseFieldDecl(id, levels, datatype, localization, domain, numSlots, initial) })
  lazy val boundaryFieldDeclaration = locationize(("Field" ~> ident) ~ levelDecl.? ~ ("on" ~> "boundary") ~ ("=" ~> fieldBoundary)
    ^^ { case id ~ levels ~ _ ~ bc => L3_BoundaryFieldDecl(id, levels, bc) })

  lazy val fieldFromOther = locationize(("Field" ~> ident) ~ levelDecl.? ~ ("from" ~> genericAccess)
    ^^ { case id ~ levels ~ src => L3_FieldFromOther(id, levels, src) })

  lazy val fieldCombinationDeclaration = locationize(("FieldCombination".? ~> ident) ~ levelDecl.? ~ (":" ~> stringLit) ~ ("=" ~> repsep(genericAccess, ","))
    ^^ { case id ~ levels ~ combType ~ fields => L3_FieldCombinationDecl(id, levels, combType, fields) })

  // ######################################
  // ##### L3_FieldOverride
  // ######################################

  lazy val overrideFieldInformation = locationize(("override" ~ "bc" ~ "for") ~> ident ~ levelDecl.? ~ ("with" ~> fieldBoundary)
    ^^ { case field ~ level ~ newBC => L3_OverrideFieldBC(field, level, newBC) })

  // #############################################################################
  // ################################# KNOWLEDGE #################################
  // #############################################################################

  // ######################################
  // ##### L3_InlineKnowledge
  // ######################################

  lazy val knowledgeParameter = locationize((ident <~ "=") ~ literal ^^ { case param ~ value => L3_KnowledgeParameter(param, value) })
  lazy val inlineKnowledge = locationize(("Knowledge" ~ "{") ~> knowledgeParameter.* <~ "}" ^^ (L3_InlineKnowledge(_)))

  // #############################################################################
  // ################################## OPERATOR #################################
  // #############################################################################

  // ######################################
  // ##### L3_StencilDecl
  // ######################################

  lazy val stencilDeclaration = (
    locationize(("Operator" ~> ident) ~ levelDecl.? ~ (("from" ~ "Stencil" ~ "{") ~> stencilEntries <~ "}")
      ^^ { case id ~ levels ~ entries => L3_BaseStencilDecl(id, levels, entries) })
      ||| locationize(("Stencil" ~> ident) ~ levelDecl.? ~ ("{" ~> stencilEntries <~ "}")
      ^^ { case id ~ levels ~ entries => L3_BaseStencilDecl(id, levels, entries) })
      ||| locationize(("Operator" ~> ident) ~ levelDecl.? ~ ("from" ~> binaryexpression)
      ^^ { case id ~ levels ~ expr => L3_StencilFromExpression(id, levels, expr) }))

  lazy val stencilEntries = (
    (stencilEntry <~ ",").+ ~ stencilEntry ^^ { case entries ~ entry => entries.::(entry) }
      ||| stencilEntry.+)

  lazy val stencilEntry = (
    locationize((constIndex ~ ("=>" ~> binaryexpression)) ^^ { case offset ~ coeff => L3_StencilOffsetEntry(offset, coeff) })
      ||| locationize(((expressionIndex <~ "from") ~ expressionIndex ~ ("with" ~> binaryexpression)) ^^ { case row ~ col ~ coeff => L3_StencilMappingEntry(row, col, coeff) }))

  lazy val stencilFromDefault = (
    locationize((("Stencil" ||| "Operator") ~> ident) ~ levelDecl.? ~ (("from" ~ "default" ~ "restriction" ~ "on") ~> localization) ~ ("with" ~> stringLit)
      ^^ { case id ~ level ~ local ~ interpolation => L3_DefaultRestrictionStencil(id, level, local, interpolation) })
      ||| locationize((("Stencil" ||| "Operator") ~> ident) ~ levelDecl.? ~ (("from" ~ "default" ~ "prolongation" ~ "on") ~> localization) ~ ("with" ~> stringLit)
      ^^ { case id ~ level ~ local ~ interpolation => L3_DefaultProlongationStencil(id, level, local, interpolation) }))

  // ######################################
  // ##### L3_StencilTemplateDecl
  // ######################################

  lazy val stencilTemplateDeclaration = locationize(("Operator" ~> ident) ~ levelDecl.? ~ (("from" ~ "StencilTemplate" ~ "on") ~> localization) ~ ("of" ~> ident) ~ ("{" ~> stencilTemplateEntries <~ "}")
    ^^ { case id ~ levels ~ local ~ domain ~ entries => L3_StencilFieldDecl(id, levels, local, domain, entries) })
  lazy val stencilTemplateEntries = (
    (stencilTemplateEntry <~ ",").+ ~ stencilTemplateEntry ^^ { case entries ~ entry => entries.::(entry) }
      ||| stencilTemplateEntry.+)
  lazy val stencilTemplateEntry = (stencilEntry
    ||| locationize((constIndex <~ "=>") ^^ { offset => L3_StencilOffsetEntry(offset, L3_NullExpression) }))

  // #############################################################################
  // ################################### SOLVER ##################################
  // #############################################################################

  /// L3_EquationDecl

  lazy val equationExpression = locationize((binaryexpression <~ "==") ~ binaryexpression ^^ { case lhs ~ rhs => L3_Equation(lhs, rhs) })
  lazy val equationDeclaration = locationize(("Equation" ~> ident) ~ levelDecl.? ~ ("{" ~> equationExpression <~ "}")
    ^^ { case id ~ levels ~ eq => L3_EquationDecl(id, levels, eq) })

  /// L3_LocalSolve

  lazy val solveLocallyComponent = /*locationize*/ (genericAccess <~ "=>") ~ equationExpression ^^ { case f ~ eq => (f, eq) }
  lazy val solveLocallyStatement = locationize((("solve" ~ "locally") ~> ("at" ~> genericAccess) ~ ("with" ~> "jacobi").? ~ ("relax" ~> binaryexpression).? <~ "{") ~ solveLocallyComponent.+ <~ "}"
    ^^ { case fieldForLoop ~ jac ~ relax ~ stmts => L3_LocalSolve(stmts.map(_._1), stmts.map(_._2), jac.isDefined, relax, fieldForLoop) })

  /// L3_SolverForEquation

  lazy val solverForEqConfig = (ident <~ "=") ~ literal ^^ { case param ~ value => (param, value) }
  lazy val solverForEqConfigs = solverForEqConfig.*
  lazy val solverForEqEntry = locationize((ident <~ "in") ~ ident ^^ { case sol ~ equation => L3_SolverForEqEntry(sol, equation) })
  lazy val solverForEq = locationize(("generate" ~ "solver" ~ "for") ~> (solverForEqEntry <~ "and").* ~ solverForEqEntry
    ~ (("with" ~ "{") ~> solverForEqConfigs <~ "}").?
    ~ (("modifiers" ~ "{") ~> solverModification.* <~ "}").?
    ~ smootherHint.*
    ^^ { case entries ~ tail ~ options ~ modifiers ~ smoother =>
    L3_SolverForEquation(entries :+ tail, options.getOrElse(List()), modifiers.getOrElse(List()), smoother)
  })

  /// L3_SolverModification

  lazy val solverModification = (locationize(
    (("append" <~ "to") | ("prepend" <~ "to") | "replace") ~ stringLit ~ levelDecl.? ~ ("{" ~> statement.* <~ "}")
      ^^ { case modification ~ target ~ levels ~ statements => L3_SolverModificationForStage(modification, target, statements, levels) })
    ||| locationize("replace" ~ stringLit ~ levelDecl.? ~ ("with" ~> genericAccess)
    ^^ { case modification ~ target ~ levels ~ access => L3_SolverModificationForObject(modification, target, access, levels) }))

  lazy val smootherHint = locationize(((("smootherHint" ||| "smootherStage") ~ "{") ~> ("loopBase" ~> genericAccess).?) ~ ((("solveFor" ~ "{") ~> genericAccess.* <~ "}").? <~ "}")
    ^^ { case loopBase ~ solveFor => L3_GenerateSmootherHint(loopBase, solveFor) })
}
