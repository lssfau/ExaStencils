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

package exastencils.parsers.l2

import scala.collection.mutable._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import exastencils.base.l2._
import exastencils.baseExt.l2._
import exastencils.boundary.l2._
import exastencils.domain.l2._
import exastencils.field.l2._
import exastencils.knowledge.l2._
import exastencils.operator.l2._
import exastencils.parsers._
import exastencils.solver.l2._
import exastencils.util.l2.L2_OffsetAlias
import exastencils.waLBerla.l2._

object L2_Parser extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = L2_Lexer

  def parse(s : String, filename : String = "") : L2_Root = {
    filenames.push(filename)
    val ret = parseTokens(new lexical.Scanner(s))
    filenames.pop()
    ret.asInstanceOf[L2_Root]
  }

  private val prevDirs = new Stack[java.io.File]().push(null)
  def parseFile(filename : String) : L2_Root = {
    val file = new java.io.File(prevDirs.top, filename)
    val lines = scala.io.Source.fromFile(file).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))

    prevDirs.push(file.getAbsoluteFile.getParentFile)
    val ret = parse(reader.source.toString, file.getName)
    prevDirs.pop()
    ret.asInstanceOf[L2_Root]
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
    import_
      ||| inlineKnowledge
      ||| domainDeclaration
      ||| fieldDeclaration
      ||| waLBerlaFieldDeclaration
      ||| fieldCombinationDeclaration
      ||| stencilDeclaration
      ||| stencilTemplateDeclaration
      ||| globals
      ||| operatorFromEq
      ||| equationDeclaration
      ||| solverHints
      ||| applicationHints
    ).* ^^ { L2_Root(_) }

  lazy val import_ = "import" ~> stringLit ^^ { parseFile }

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
      ||| fieldIteratorAccess
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
      ||| ("Float" ||| "float" ||| "Flt") ^^ { _ => L2_FloatDatatype }
      ||| ("Double" ||| "double" ||| "Dbl") ^^ { _ => L2_DoubleDatatype })

  // ######################################
  // ##### L2_VariableDeclaration
  // ######################################

  lazy val localDeclaration = variableDeclaration ||| valueDeclaration

  lazy val variableDeclaration = locationize((("Var" ||| "Variable") ~> ident) ~ levelDecl.? ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ dt ~ exp => L2_VariableDeclaration(id, levels, dt, exp, false) })

  lazy val valueDeclaration = locationize((("Val" ||| "Value") ~> ident) ~ levelDecl.? ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression))
    ^^ { case id ~ levels ~ dt ~ exp => L2_VariableDeclaration(id, levels, dt, Some(exp), true) })

  // ######################################
  // ##### L2_Function
  // ######################################

  lazy val functionReference = locationize(ident ~ levelAccess.? ~ ("@" ~> constIndex).?
    ^^ { case id ~ level ~ offset => L2_UnresolvedFunctionReference(id, level, offset) })

  lazy val functionCallArgumentList = /*locationize*/ repsep(binaryexpression ||| booleanexpression, listdelimiter)
  lazy val functionCall = locationize(functionReference ~ ("(" ~> functionCallArgumentList <~ ")") ^^ { case id ~ args => L2_FunctionCall(id, args : _*) })

  // ######################################
  // ##### L2_Index
  // ######################################

  lazy val index = expressionIndex ||| constIndex

  lazy val expressionIndex = locationize("[" ~> repsep(binaryexpression, ",") <~ "]" ^^ (l => L2_ExpressionIndex(l.toArray)))
  lazy val constIndex = (
    locationize("[" ~> repsep(integerLit, ",") <~ "]" ^^ (l => L2_ConstIndex(l.toArray)))
      ||| ("center" ||| "east" ||| "west" ||| "north" ||| "south" ||| "top" ||| "bottom") ^^ (s => L2_OffsetAlias.toConstIndex(s)))

  // ######################################
  // ##### L2_LevelSpecification
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

  lazy val allLevels = locationize("all" ^^ { _ => L2_AllLevels })

  lazy val levelDeclRange = locationize((singleDeclLevel <~ "to") ~ singleDeclLevel ^^ { case b ~ e => L2_LevelRange(b, e) })

  lazy val levelDeclList : Parser[L2_DeclarationLevelSpecification] = (
    locationize((singleDeclLevel <~ ("," ||| "and")).+ ~ singleDeclLevel ^^ { case a ~ b => L2_LevelList(a :+ b) })
      ||| locationize("(" ~> levelDeclList <~ ")") ^^ { l => l })

  lazy val levelDeclNegList : Parser[L2_LevelList] = (
    locationize((levelDeclGroup <~ ("but" ||| "not")) ~ levelDeclGroup ^^ { case in ~ out => L2_LevelList(List(in, L2_NegatedLevelList(out))) })
      ||| locationize("(" ~> levelDeclNegList <~ ")") ^^ { l => l })

  lazy val singleAccessLevel : Parser[L2_AccessLevelSpecification] = (
    directAccessLevel
      ||| relativeAccessLevel
      ||| locationize("(" ~> singleAccessLevel <~ ")") ^^ { l => l })

  lazy val singleDeclLevel : Parser[L2_DeclarationLevelSpecification] = (
    directDeclLevel
      ||| relativeDeclLevel
      ||| locationize("(" ~> singleDeclLevel <~ ")") ^^ { l => l })

  lazy val relativeAccessLevel = locationize(directAccessLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L2_RelativeLevel(l, op, i) })
  lazy val relativeDeclLevel = locationize(directDeclLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L2_RelativeLevel(l, op, i) })

  lazy val directAccessLevel : Parser[L2_AccessLevelSpecification] = (
    locationize("current" ^^ { _ => L2_CurrentLevel })
      ||| locationize("coarser" ^^ { _ => L2_CoarserLevel })
      ||| locationize("finer" ^^ { _ => L2_FinerLevel })
      ||| locationize("coarsest" ^^ { _ => L2_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L2_FinestLevel })
      ||| locationize(integerLit ^^ { l => L2_SingleLevel(l) })
      ||| locationize("(" ~> directAccessLevel <~ ")" ^^ { l => l }))

  lazy val directDeclLevel : Parser[L2_DeclarationLevelSpecification] = (
    locationize("coarsest" ^^ { _ => L2_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L2_FinestLevel })
      ||| locationize(integerLit ^^ { l => L2_SingleLevel(l) })
      ||| locationize("(" ~> directDeclLevel <~ ")" ^^ { l => l }))

  // #############################################################################
  // ################################## BASE_EXT #################################
  // #############################################################################

  // ######################################
  // ##### L2_ApplicationHints
  // ######################################

  lazy val applicationHint = applicationParameter
  lazy val applicationParameter = locationize((ident <~ "=") ~ literal ^^ { case param ~ value => L2_ApplicationParameter(param, value) })
  lazy val applicationHints = locationize((("ApplicationHint" ||| "ApplicationHints" ||| "L4Hint" ||| "L4Hints") ~ "{") ~> applicationHint.* <~ "}"
    ^^ (L2_ApplicationHints(_)))

  // ######################################
  // ##### L2_ExpressionDeclaration
  // ######################################

  lazy val expressionDeclaration = locationize((("Expr" ||| "Expression") ~> ident) ~ levelDecl.? ~ ("=" ~> (binaryexpression ||| booleanexpression))
    ^^ { case id ~ levels ~ exp => L2_ExpressionDeclaration(id, levels, exp) })

  // ######################################
  // ##### L2_FieldIteratorAccess
  // ######################################

  lazy val fieldIteratorAccess = (
    locationize(("i0" | "i1" | "i2") ^^ { id => L2_FieldIteratorAccess(id) })
      ||| locationize(("x" | "y" | "z") ^^ { id => L2_FieldIteratorAccess(id) })
    )

  // ######################################
  // ##### L2_GlobalSection
  // ######################################

  lazy val globals = locationize(("Globals" ~> "{" ~> globalEntry.* <~ "}") ^^ { L2_GlobalSection(_) })
  lazy val globalEntry = locationize(valueDeclaration ||| variableDeclaration ||| expressionDeclaration)

  // ######################################
  // ##### L2_HigherOrderDatatype
  // ######################################

  lazy val higherOrderDatatype : Parser[L2_Datatype] = (
    "Vector" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L2_VectorDatatype(x, s) }
      ||| ("RowVector" ||| "RVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L2_VectorDatatype(x, s, true) }
      ||| ("ColumnVector" ||| "CVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L2_VectorDatatype(x, s, false) }
      ||| numericDatatype ~ ("<" ~> integerLit <~ ">") ^^ { case x ~ s => L2_VectorDatatype(x, s) }
      ||| "Vec2" ^^ { _ => L2_VectorDatatype(L2_RealDatatype, 2) }
      ||| "Vec3" ^^ { _ => L2_VectorDatatype(L2_RealDatatype, 3) }
      ||| "Vec4" ^^ { _ => L2_VectorDatatype(L2_RealDatatype, 4) }
      ||| "Matrix" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ m ~ n => L2_MatrixDatatype(x, m, n) }
      ||| numericDatatype ~ ("<" ~> integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case x ~ m ~ n => L2_MatrixDatatype(x, m, n) })

  // ######################################
  // ##### L2_UnresolvedAccess
  // ######################################

  lazy val genericAccess = locationize(ident ~ levelAccess.? ~ ("@" ~> constIndex).? ~ (":" ~> constIndex).?
    ^^ { case id ~ level ~ offset ~ dirAccess => L2_UnresolvedAccess(id, level, None, offset, dirAccess, None) })

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

  lazy val realIndex = /*locationize*/ "[" ~> realLit ~ ("," ~> realLit).* <~ "]" ^^ { case b ~ l => (List(b) ++ l).toArray }
  lazy val domainDeclaration = (
    locationize(("Domain".? ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l ~ u => L2_DomainFromAABBDecl(id, l, u) })
      ||| locationize(("Domain".? ~> ident) ~ (" from " ~> realIndex <~ "to") ~ realIndex ^^ { case id ~ l ~ u => L2_DomainFromAABBDecl(id, l, u) }))

  // #############################################################################
  // #################################### FIELD ##################################
  // #############################################################################

  // ######################################
  // ##### l2_FieldDeclarations
  // ######################################

  lazy val fieldDeclaration = baseFieldDeclaration ||| boundaryFieldDeclaration ||| fieldFromOther

  lazy val localization = ("Node" ||| "node" ||| "Cell" ||| "cell"
    ||| "Face_x" ||| "face_x" ||| "Face_y" ||| "face_y" ||| "Face_z" ||| "face_z"
    ||| "Edge_Node" ||| "edge_node" ||| "Edge_Cell" ||| "edge_cell"
    ^^ (l => l))

  lazy val baseFieldDeclaration = locationize(("Field".? ~> ident) ~ levelDecl.? ~ ("with" ~> datatype).? ~ ("on" ~> localization) ~ ("of" ~> ident) ~ (integerLit <~ "times").? ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ datatype ~ localization ~ domain ~ numSlots ~ initial => L2_BaseFieldDecl(id, levels, datatype, localization, domain, numSlots, initial) })
  lazy val boundaryFieldDeclaration = locationize(("Field".? ~> ident) ~ levelDecl.? ~ ("on" ~> "boundary") ~ ("=" ~> fieldBoundary)
    ^^ { case id ~ levels ~ _ ~ bc => L2_BoundaryFieldDecl(id, levels, bc) })

  lazy val fieldFromOther = locationize(("Field" ~> ident) ~ levelDecl.? ~ ("from" ~> genericAccess)
    ^^ { case id ~ levels ~ src => L2_FieldFromOther(id, levels, src) })

  lazy val fieldCombinationDeclaration = locationize(("FieldCombination".? ~> ident) ~ levelDecl.? ~ (":" ~> stringLit) ~ ("=" ~> repsep(genericAccess, ","))
    ^^ { case id ~ levels ~ combType ~ fields => L2_FieldCombinationDecl(id, levels, combType, fields) })


  // ######################################
  // ##### l2_WaLBerlaFieldDeclarations
  // ######################################

  lazy val waLBerlaFieldDeclaration = waLBerlaBaseFieldDeclaration ||| waLBerlaBoundaryFieldDeclaration ||| waLBerlaFieldFromOther

  lazy val waLBerlaBaseFieldDeclaration = locationize(("waLBerla" ~ "Field" ~> ident) ~ levelDecl.? ~ ("with" ~> datatype).? ~ (integerLit <~ "times").? ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ datatype ~ numSlots ~ initial => L2_WaLBerlaBaseFieldDecl(id, levels, datatype, numSlots, initial) })

  lazy val waLBerlaBoundaryFieldDeclaration = locationize("waLBerla" ~> boundaryFieldDeclaration
    ^^ ( bcFieldDecl => L2_WaLBerlaBoundaryFieldDecl(bcFieldDecl.name, bcFieldDecl.levels, bcFieldDecl.boundary)))

  lazy val waLBerlaFieldFromOther = locationize("waLBerla" ~> fieldFromOther
    ^^ ( fieldFromOtherDecl => L2_WaLBerlaFieldFromOther(fieldFromOtherDecl.name, fieldFromOtherDecl.levels, fieldFromOtherDecl.src) ))

  // #############################################################################
  // ################################# KNOWLEDGE #################################
  // #############################################################################

  // ######################################
  // ##### L2_InlineKnowledge
  // ######################################

  lazy val knowledgeParameter = locationize((ident <~ "=") ~ literal ^^ { case param ~ value => L2_KnowledgeParameter(param, value) })
  lazy val inlineKnowledge = locationize(("Knowledge" ~ "{") ~> knowledgeParameter.* <~ "}" ^^ (L2_InlineKnowledge(_)))

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

  lazy val stencilDeclaration = (
    locationize(("Operator".? ~> ident) ~ levelDecl.? ~ (("from" ~ "Stencil" ~ "{") ~> stencilEntries <~ "}")
      ^^ { case id ~ levels ~ entries => L2_BaseStencilDecl(id, levels, entries) })
      ||| locationize(("Operator".? ~> ident) ~ levelDecl.? ~ ("from" ~> binaryexpression)
      ^^ { case id ~ levels ~ expr => L2_StencilFromExpression(id, levels, expr) })
      ||| stencilFromDefault)

  lazy val stencilEntries = (
    (stencilEntry <~ ",").+ ~ stencilEntry ^^ { case entries ~ entry => entries.::(entry) }
      ||| stencilEntry.+)

  lazy val stencilEntry = (
    locationize((constIndex ~ ("=>" ~> binaryexpression)) ^^ { case offset ~ coeff => L2_StencilOffsetEntry(offset, coeff) })
      ||| locationize(((expressionIndex <~ "from") ~ expressionIndex ~ ("with" ~> binaryexpression)) ^^ { case row ~ col ~ coeff => L2_StencilMappingEntry(row, col, coeff) }))

  lazy val stencilFromDefault = (
    locationize((("Stencil" ||| "Operator").? ~> ident) ~ levelDecl.? ~ (("from" ~ "default" ~ "restriction" ~ "on") ~> localization) ~ ("with" ~> stringLit)
      ^^ { case id ~ level ~ local ~ interpolation => L2_DefaultRestrictionStencil(id, level, local, interpolation) })
      ||| locationize((("Stencil" ||| "Operator").? ~> ident) ~ levelDecl.? ~ (("from" ~ "default" ~ "prolongation" ~ "on") ~> localization) ~ ("with" ~> stringLit)
      ^^ { case id ~ level ~ local ~ interpolation => L2_DefaultProlongationStencil(id, level, local, interpolation) }))

  // ######################################
  // ##### L2_StencilTemplateDecl
  // ######################################

  lazy val stencilTemplateDeclaration = locationize(("Operator".? ~> ident) ~ levelDecl.? ~ (("from" ~ "StencilTemplate" ~ "on") ~> localization)
    ~ ("of" ~> ident) ~ ("{" ~> stencilTemplateEntries <~ "}")
    ^^ { case id ~ levels ~ local ~ domain ~ entries => L2_StencilFieldDecl(id, levels, local, domain, entries) })
  lazy val stencilTemplateEntries = (
    (stencilTemplateEntry <~ ",").+ ~ stencilTemplateEntry ^^ { case entries ~ entry => entries.::(entry) }
      ||| stencilTemplateEntry.+)
  lazy val stencilTemplateEntry = (stencilEntry
    ||| locationize((constIndex <~ "=>") ^^ { offset => L2_StencilOffsetEntry(offset, L2_NullExpression) }))

  // #############################################################################
  // ################################### SOLVER ##################################
  // #############################################################################

  lazy val equationDeclaration = locationize(("Equation".? ~> ident) ~ levelDecl.? ~ ("{" ~> equation <~ "}")
    ^^ { case id ~ levels ~ eq => L2_EquationDecl(id, levels, eq) })

  // ######################################
  // ##### L2_SolverForEquation
  // ######################################

  lazy val solverForEqEntry = locationize((ident <~ "in") ~ ident ^^ { case unknownName ~ eqName => L2_SolverForEqEntry(unknownName, eqName) })
  lazy val solverForEq = locationize(("generate" ~ "solver" ~ "for") ~> (solverForEqEntry <~ "and").* ~ solverForEqEntry
    ^^ { case entries ~ tail => L2_SolverForEquation(entries :+ tail) })

  // ######################################
  // ##### L2_SolverHints
  // ######################################

  lazy val solverHint = solverForEq ||| solverParameter

  lazy val solverParameter = locationize((ident <~ "=") ~ literal ^^ { case param ~ value => L2_SolverParameter(param, value) })

  lazy val solverHints = locationize((("Solve" ||| "SolverHint" ||| "SolverHints" ||| "L3Hint" ||| "L3Hints") ~ "{") ~> solverHint.* <~ "}"
    ^^ (L2_SolverHints(_)))

  /// TO BE INTEGRATED

  lazy val equation = locationize((binaryexpression <~ ("=" | "==")) ~ binaryexpression ^^ { case lhs ~ rhs => L2_Equation(lhs, rhs) })
  lazy val operatorMapping = locationize((genericAccess <~ "=>") ~ ident ~ levelDecl.? ^^ { case field ~ name ~ level => L2_OperatorMapping(name, level, field) })

  lazy val operatorFromInlineEq = locationize((("equation" ~ "for") ~> genericAccess) ~ ("{" ~> equation <~ "}") ~ (("store" ~ "in" ~ "{") ~> operatorMapping.* <~ "}")
    ^^ { case field ~ eq ~ map => L2_OperatorFromInlineEq(field, eq, map) })

  lazy val operatorFromNamedEq = locationize((("equation" ~ "for") ~> genericAccess) ~ ("is" ~> genericAccess) ~ (("store" ~ "in" ~ "{") ~> operatorMapping.* <~ "}")
    ^^ { case field ~ eq ~ map => L2_OperatorFromNamedEq(field, eq, map) })

  lazy val operatorFromEqEntry = operatorFromInlineEq | operatorFromNamedEq

  lazy val operatorFromEq = locationize((("generate" ~ "operators") ~> levelDecl.?) ~ ("{" ~> operatorFromEqEntry.* <~ "}")
    ^^ { case level ~ entries => L2_OperatorFromEquation(level, entries) })
}
