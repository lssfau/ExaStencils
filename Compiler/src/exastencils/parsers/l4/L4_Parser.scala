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

package exastencils.parsers.l4

import scala.collection.mutable._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input._

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_ComplexExpression
import exastencils.baseExt.l4._
import exastencils.boundary.l4._
import exastencils.communication.l4._
import exastencils.datastructures._
import exastencils.domain.l4._
import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ExternalFieldDecl
import exastencils.knowledge.l4._
import exastencils.layoutTransformation.l4._
import exastencils.logger.Logger
import exastencils.operator.l4._
import exastencils.parsers._
import exastencils.solver.l4._
import exastencils.util.l4.L4_OffsetAlias

/// L4_Parser

//noinspection TypeAnnotation,VariablePatternShadow
object L4_Parser extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = new L4_Lexer()

  def parse(s : String, filename : String = "") : L4_Root = {
    filenames.push(filename)
    val ret = parseTokens(new lexical.Scanner(s))
    filenames.pop()
    ret.asInstanceOf[L4_Root]
  }

  private var prevDirs = List[java.io.File](null)
  def parseFile(filename : String) : L4_Root = {
    val file = new java.io.File(prevDirs.head, filename)
    val lines = scala.io.Source.fromFile(file).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))

    prevDirs = file.getAbsoluteFile.getParentFile :: prevDirs
    val ret = parse(reader.source.toString, file.getName)
    prevDirs = prevDirs.tail
    ret.asInstanceOf[L4_Root]
  }

  protected def parseTokens(tokens : lexical.Scanner) : Node = {
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
      ||| domain
      ||| layout
      ||| field
      ||| fieldCombinationDeclaration
      ||| stencilField
      ||| externalField
      ||| stencilDeclaration
      ||| stencilFromDefault
      ||| equationDeclaration
      ||| globals
      ||| layoutTrafos
      ||| function
      ||| functionTemplate
      ||| functionInstantiation
    ).* ^^ { L4_Root(_) }

  lazy val import_ = "import" ~> stringLit ^^ { parseFile }

  // ######################################
  // ##### Level Specifications
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

  lazy val allLevels = locationize("all" ^^ { _ => L4_AllLevels })

  lazy val levelDeclRange = locationize((singleDeclLevel <~ "to") ~ singleDeclLevel ^^ { case b ~ e => L4_LevelRange(b, e) })

  lazy val levelDeclList : Parser[L4_DeclarationLevelSpecification] = (
    locationize(repsep(singleDeclLevel, "," ||| "and") ^^ { x => L4_LevelList(x) })
      ||| locationize("(" ~> levelDeclList <~ ")"))

  lazy val levelDeclNegList : Parser[L4_LevelList] = (
    locationize((levelDeclGroup <~ ("but" ||| "not")) ~ levelDeclGroup ^^ { case in ~ out => L4_LevelList(List(in, L4_NegatedLevelList(out))) })
      ||| locationize("(" ~> levelDeclNegList <~ ")"))

  lazy val singleAccessLevel : Parser[L4_AccessLevelSpecification] = (
    directAccessLevel
      ||| relativeAccessLevel
      ||| locationize("(" ~> singleAccessLevel <~ ")"))

  lazy val singleDeclLevel : Parser[L4_DeclarationLevelSpecification] = (
    directDeclLevel
      ||| relativeDeclLevel
      ||| locationize("(" ~> singleDeclLevel <~ ")"))

  lazy val relativeAccessLevel = locationize(directAccessLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L4_RelativeLevel(l, op, i) })
  lazy val relativeDeclLevel = locationize(directDeclLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L4_RelativeLevel(l, op, i) })

  lazy val directAccessLevel : Parser[L4_AccessLevelSpecification] = (
    locationize("current" ^^ { _ => L4_CurrentLevel })
      ||| locationize("coarser" ^^ { _ => L4_CoarserLevel })
      ||| locationize("finer" ^^ { _ => L4_FinerLevel })
      ||| locationize("coarsest" ^^ { _ => L4_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L4_FinestLevel })
      ||| locationize(integerLit ^^ { l => L4_SingleLevel(l) })
      ||| locationize("(" ~> directAccessLevel <~ ")" ^^ { l => l }))

  lazy val directDeclLevel : Parser[L4_DeclarationLevelSpecification] = (
    locationize("coarsest" ^^ { _ => L4_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L4_FinestLevel })
      ||| locationize(integerLit ^^ { l => L4_SingleLevel(l) })
      ||| locationize("(" ~> directDeclLevel <~ ")"))

  // ######################################
  // ##### Datatypes
  // ######################################

  lazy val datatype : Parser[L4_Datatype] = simpleDatatype ||| algorithmicDatatype

  lazy val simpleDatatype : Parser[L4_Datatype] = (
    "String" ^^ { _ => L4_StringDatatype }
      ||| ("Boolean" ||| "Bool") ^^ { _ => L4_BooleanDatatype }
      ||| numericDatatype)

  lazy val algorithmicDatatype : Parser[L4_Datatype] = (
    ("Complex" ~ "<") ~> numericDatatype <~ ">" ^^ { x => L4_ComplexDatatype(x) }
      ||| "Vector" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L4_VectorDatatype(x, s) }
      ||| ("RowVector" ||| "RVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L4_VectorDatatype(x, s, true) }
      ||| ("ColumnVector" ||| "CVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L4_VectorDatatype(x, s, false) }
      ||| numericDatatype ~ ("<" ~> integerLit <~ ">") ^^ { case x ~ s => L4_VectorDatatype(x, s) }
      ||| "Vec2" ^^ { _ => L4_VectorDatatype(L4_RealDatatype, 2) }
      ||| "Vec3" ^^ { _ => L4_VectorDatatype(L4_RealDatatype, 3) }
      ||| "Vec4" ^^ { _ => L4_VectorDatatype(L4_RealDatatype, 4) }
      ||| "Matrix" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ m ~ n => L4_MatrixDatatype(x, m, n) }
      ||| numericDatatype ~ ("<" ~> integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case x ~ m ~ n => L4_MatrixDatatype(x, m, n) }
      ||| numericDatatype)

  lazy val numericDatatype : Parser[L4_Datatype] = (
    ("Integer" ||| "integer" ||| "Int" ||| "int") ^^ { _ => L4_IntegerDatatype }
      ||| ("Real" ||| "real") ^^ { _ => L4_RealDatatype }
      ||| ("Float" ||| "float" ||| "Flt") ^^ { _ => L4_FloatDatatype }
      ||| ("Double" ||| "double" ||| "Dbl") ^^ { _ => L4_DoubleDatatype })

  lazy val returnDatatype = ("Unit" ^^ { _ => L4_UnitDatatype }
    ||| datatype)

  // ######################################
  // ##### Functions
  // ######################################

  lazy val function = locationize("noinline".? ~ (("Func" ||| "Function") ~> ident) ~ levelDecl.? ~ ("(" ~> repsep(functionArgument, ",").? <~ ")").? ~ (":" ~> returnDatatype).? ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case inline ~ id ~ level ~ args ~ t ~ stmts => L4_FunctionDecl(id, level, t, args, stmts, inline.isEmpty) })
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => L4_Function.Argument(id, t) })
  lazy val functionReference = locationize(ident ~ levelAccess.? ~ ("@" ~> constIndex).? ^^ { case id ~ level ~ offset => L4_UnresolvedFunctionReference(id, level, offset) })
  lazy val functionCall = locationize(functionReference ~ ("(" ~> repsep(binaryexpression ||| booleanexpression, ",").? <~ ")")
    ^^ { case id ~ args => L4_FunctionCall(id, args.getOrElse(List()).to[ListBuffer]) })

  lazy val functionTemplate = locationize((("FuncTemplate" ||| "FunctionTemplate") ~> ident) ~ ("<" ~> repsep(ident, ",").? <~ ">") ~ ("(" ~> repsep(functionArgument, ",").? <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case id ~ templateArgs ~ functionArgs ~ retType ~ stmts => L4_FunctionTemplate(id, retType, templateArgs.getOrElse(List()), functionArgs.getOrElse(List()), stmts) })
  lazy val functionInstantiation = locationize(((("Inst" ||| "Instantiate") ~> ident) ~ ("<" ~> repsep(binaryexpression ||| booleanexpression, ",").? <~ ">") ~ ("as" ~> ident) ~ levelDecl.?)
    ^^ { case template ~ args ~ target ~ targetLvl => L4_FunctionInstantiation(template, args.getOrElse(List()), target, targetLvl) })

  // ######################################
  // ##### Statements
  // ######################################

  lazy val statement : Parser[L4_Statement] = (
    variableDeclaration
      ||| valueDeclaration
      ||| expressionDeclaration
      ||| repeatNTimes
      ||| contractionLoop
      ||| repeatUntil
      ||| repeatWhile
      ||| loopOver
      ||| loopOverFragments
      ||| assignment
      ||| operatorassignment
      ||| locationize(functionCall ^^ { L4_ExpressionStatement(_) })
      ||| conditional
      ||| applyBCsStatement
      ||| communicateStatement
      ||| returnStatement
      ||| advanceStatement
      ||| levelScope
      ||| solveLocallyStatement
      ||| colorWithStatement
      ||| repeatWithStatement)

  lazy val statementInsideRepeat = statement ||| breakStatement

  lazy val variableDeclaration = locationize((("Var" ||| "Variable") ~> ident) ~ levelDecl.? ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ dt ~ exp => L4_VariableDeclaration(id, levels, dt, exp, false) })

  lazy val valueDeclaration = locationize((("Val" ||| "Value") ~> ident) ~ levelDecl.? ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression))
    ^^ { case id ~ levels ~ dt ~ exp => L4_VariableDeclaration(id, levels, dt, Some(exp), true) })

  lazy val repeatNTimes = locationize(("repeat" ~> numericLit <~ "times") ~ ("count" ~> (flatAccess ||| leveledAccess)).? ~ ("{" ~> statementInsideRepeat.* <~ "}") ^^ { case n ~ i ~ s => L4_ForLoop(n.toInt, i, s) })
  lazy val contractionLoop = locationize(("repeat" ~> numericLit <~ "times") ~ ("count" ~> (flatAccess ||| leveledAccess)).? ~ contractionClause ~ ("{" ~> statementInsideRepeat.* <~ "}") ^^ { case n ~ i ~ c ~ s => L4_ContractingLoop(n.toInt, i, c, s) })
  lazy val contractionClause = locationize("with" ~ "contraction" ~> constIndex ~ ("," ~> constIndex).? ^^ { case l ~ r => L4_ContractionSpecification(l, r) })

  lazy val repeatUntil = locationize((("repeat" ~ "until") ~> booleanexpression) ~ (("{" ~> statementInsideRepeat.*) <~ "}") ^^ { case c ~ s => L4_UntilLoop(c, s.to[ListBuffer]) })
  lazy val repeatWhile = locationize((("repeat" ~ "while") ~> booleanexpression) ~ (("{" ~> statementInsideRepeat.*) <~ "}") ^^ { case c ~ s => L4_WhileLoop(c, s.to[ListBuffer]) })

  lazy val breakStatement = locationize("break" ^^ (_ => L4_Break()))

  lazy val loopOverFragments = locationize(("loop" ~ "over" ~ "fragments") ~ ("with" ~> reductionClause).? ~ ("{" ~> statement.* <~ "}") ^^ { case _ ~ red ~ stmts => L4_LoopOverFragments(stmts, red) })
  //  lazy val loopOver = locationize(("loop" ~ "over" ~> genericAccess) ~ //fieldAccess
  //    ("only" ~> regionSpecification).? ~
  //    "sequentially".? ~ // FIXME: seq HACK
  //    ("where" ~> booleanexpression).? ~
  //    ("starting" ~> expressionIndex).? ~
  //    ("ending" ~> expressionIndex).? ~
  //    ("stepping" ~> expressionIndex).? ~
  //    ("with" ~> reductionClause).? ~
  //    precomm.* ~
  //    postcomm.* ~
  //    ("{" ~> statement.* <~ "}") ^^ {
  //    case field ~ region ~ seq ~ cond ~ startOff ~ endOff ~ inc ~ red ~ prec ~ postc ~ stmts =>
  //      L4_LoopOverField(field, region, seq.isDefined, cond, startOff, endOff, inc, stmts, red, prec, postc)
  //  })
  lazy val loopOver = locationize(("loop" ~ "over" ~> genericAccess) ~ //fieldAccess
    loopModifier.* ~
    ("{" ~> statement.* <~ "}") ^^ {
    case field ~ modifiers ~ stmts =>
      L4_LoopOverField(field, modifiers, stmts)
  })
  lazy val loopModifier : Parser[(String, Any)] = (
    "only" ~> regionSpecification ^^ (p => ("only", p))
      ||| "sequentially" ^^ (_ => ("sequentially", true)) // FIXME: seq HACK
      ||| "where" ~> booleanexpression ^^ (p => ("where", p))
      ||| "starting" ~> expressionIndex ^^ (p => ("starting", p))
      ||| "ending" ~> expressionIndex ^^ (p => ("ending", p))
      ||| "stepping" ~> expressionIndex ^^ (p => ("stepping", p))
      ||| "with" ~> reductionClause ^^ (p => ("with", p))
      ||| precomm ^^ (p => ("precomm", p))
      ||| postcomm ^^ (p => ("postcomm", p))
    )
  lazy val reductionClause = locationize((("reduction" ~ "(") ~> (ident ||| "+" ||| "*")) ~ (":" ~> ident <~ ")") ^^ { case op ~ s => L4_Reduction(op, s) })
  lazy val regionSpecification = locationize((("ghost" ||| "dup" ||| "inner") ~ constIndex ~ ("on" <~ "boundary").?) ^^ { case region ~ dir ~ bc => L4_RegionSpecification(region, dir, bc.isDefined) })

  lazy val assignment = locationize(genericAccess ~ "=" ~ (binaryexpression ||| booleanexpression) ^^ { case id ~ op ~ exp => L4_Assignment(id, exp, op) })
  lazy val operatorassignment = locationize(genericAccess ~ ("+=" ||| "-=" ||| "*=" ||| "/=") ~ binaryexpression
    ^^ { case id ~ op ~ exp => L4_Assignment(id, exp, op) })

  lazy val conditional : Parser[L4_IfCondition] = (
    locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.* <~ "}") ~ (("else" ~ "{") ~> statement.* <~ "}").?
      ^^ { case exp ~ stmts ~ elsestmts => L4_IfCondition(exp, stmts.to[ListBuffer], elsestmts.getOrElse(List()).to[ListBuffer]) })
      ||| locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.* <~ "}") ~ ("else" ~> conditional)
      ^^ { case exp ~ stmts ~ elsecond => L4_IfCondition(exp, stmts.to[ListBuffer], ListBuffer[L4_Statement](elsecond)) }))

  lazy val applyBCsStatement = locationize(("apply" ~ "bc" ~ "to") ~> genericAccess //fieldAccess
    ^^ (field => L4_ApplyBC(field)))
  lazy val communicateStatement = locationize((("begin" ||| "finish").? <~ ("communicate" ||| "communicating")) ~ communicateTarget.* ~ ("of".? ~> genericAccess) //fieldAccess
    ~ ("where" ~> booleanexpression).?
    ^^ { case op ~ targets ~ field ~ cond => L4_Communicate(field, op.getOrElse("both"), targets, cond) })
  lazy val communicateTarget = locationize(("all" ||| "dup" ||| "ghost") ~ constIndex.? ~ ("to" ~> constIndex).? // inclusive indices
    ^^ { case target ~ start ~ end => L4_CommunicateTarget(target, start, end) })
  lazy val precomm = locationize("precomm" ~> ("begin" ||| "finish").? ~ communicateTarget.* ~ ("of".? ~> genericAccess) ~ ("where" ~> booleanexpression).?
    ^^ { case op ~ targets ~ field ~ cond => L4_Communicate(field, op.getOrElse("both"), targets, cond) })
  lazy val postcomm = locationize("postcomm" ~> ("begin" ||| "finish").? ~ communicateTarget.* ~ ("of".? ~> genericAccess) ~ ("where" ~> booleanexpression).?
    ^^ { case op ~ targets ~ field ~ cond => L4_Communicate(field, op.getOrElse("both"), targets, cond) })

  lazy val returnStatement = locationize("return" ~> (binaryexpression ||| booleanexpression).? ^^ (exp => L4_Return(exp)))

  lazy val levelScope = locationize(((levelDecl ||| levelAccess) <~ "{") ~ (statement.* <~ "}") ^^ { case l ~ s => L4_LevelScope(l, s) })

  lazy val solveLocallyComponent = /*locationize*/ (genericAccess <~ "=>") ~ equationExpression ^^ { case f ~ eq => (f, eq) }
  lazy val solveLocallyStatement = locationize((("solve" ~ "locally") ~> ("with" ~> "jacobi").? ~ ("relax" ~> binaryexpression).? <~ "{") ~ solveLocallyComponent.+ <~ "}"
    ^^ { case jac ~ relax ~ stmts => L4_LocalSolve(stmts.map(_._1), stmts.map(_._2), jac.isDefined, relax) })

  lazy val repeatWithStatement = locationize(("repeat" ~ "with" ~ "{") ~> (booleanexpression <~ ",").+ ~ statement.* <~ "}"
    ^^ { case conds ~ stmts => L4_RepeatLoops(conds.to, stmts.to) })

  lazy val colorWithStatement = locationize(("color" ~ "with" ~ "{") ~> (colorDefExpression <~ ",").+ ~ statement.* <~ "}"
    ^^ { case colors ~ stmts => L4_ColorLoops(colors.to, stmts.to) })
  lazy val colorDefExpression : Parser[L4_Modulo] = locationize(binaryexpression ^^ {
    case modExp @ L4_Modulo(_, L4_IntegerConstant(_)) =>
      modExp
    case exp                                          =>
      Logger.error("color expression in 'color with' statement must be a modulo expression with a constant integral divisor" + exp.location.toAppendString)
  })

  // ######################################
  // ##### Globals
  // ######################################

  lazy val globals = locationize(("Globals" ~> "{" ~> globalEntry.* <~ "}") ^^ { L4_GlobalSection(_) })
  lazy val globalEntry = locationize(valueDeclaration ||| variableDeclaration ||| expressionDeclaration)

  // ######################################
  // ##### Layout Transformations
  // ######################################

  lazy val layoutTrafos = locationize(("LayoutTransformations" ~> "{" ~> layoutEntry.* <~ "}") ^^ { stmts => L4_LayoutSection(stmts.to) })
  lazy val layoutEntry : Parser[L4_LayoutTransformStatement] = locationize(aliasStmt ||| transformStmt ||| concatFieldsStmt)
  lazy val aliasStmt = locationize(("rename" ~> idWithLevelDecl) ~ ("to" ~> ident)
    ^^ { case old ~ nju => L4_ExternalFieldAlias(nju, old) })
  lazy val transformStmt = locationize(("transform" ~> repsep(idWithLevelDecl, "and" ||| ",")) ~ ("with" ~> "[" ~> repsep(genericAccess ||| fieldIteratorAccess, ",") <~ "]") ~ ("=>" ~> expressionIndex) ^^ {
    case fieldNames ~ its ~ trafo =>
      L4_GenericTransform(fieldNames, its.view.map { // HACK: to allow x, y, z as identifiers (they will be renamed in expressionIndex, so ensure they are renamed here, too)
        case a : L4_UnresolvedAccess    => L4_PlainVariableAccess(a.name, L4_IntegerDatatype, true)
        case a : L4_PlainVariableAccess => a
      }.toArray, trafo)
  })
  lazy val idWithLevelDecl = ident ~ levelDecl.? ^^ { case id ~ lvls => L4_LeveledIdentifier(id, lvls) }
  lazy val concatFieldsStmt = locationize("concat" ~> levelDecl.? ~ repsep(ident, "and" ||| ",") ~ ("into" ~> ident)
    ^^ { case lvls ~ fields ~ nju => L4_FieldConcatenation(nju, fields, lvls) })

  // ######################################
  // ##### Object Declarations
  // ######################################

  lazy val realIndex = /*locationize*/ "[" ~> repsep(realLit, ",") <~ "]" ^^ { l => l.toArray }
  lazy val domain = locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l ~ u => L4_DomainFromAABBDecl(id, l, u) })

  lazy val layout = locationize(("Layout" ~> ident) ~ ("<" ~> datatype <~ ",") ~ (localization <~ ">") ~ levelDecl.? ~ ("{" ~> repsep(layoutOption, ",".?) <~ "}")
    ^^ { case id ~ dt ~ disc ~ level ~ opts => L4_FieldLayoutDecl(id, level, dt, disc.toLowerCase, opts) })
  lazy val layoutOption = locationize((ident <~ "=") ~ constIndex ~ ("with" ~ "communication").?
    ^^ { case id ~ idx ~ comm => L4_FieldLayoutOption(id, idx, comm.isDefined) })

  lazy val field = locationize(("Field" ~> ident) ~ ("<" ~> ident) ~ ("," ~> ident) ~ ("," ~> fieldBoundary) ~ ">" ~ ("[" ~> integerLit <~ "]").? ~ levelDecl.?
    ^^ { case id ~ domain ~ layout ~ boundary ~ _ ~ slots ~ level => L4_BaseFieldDecl(id, level, domain, layout, boundary, slots.getOrElse(1)) })
  lazy val fieldBoundary = (
    "Neumann" ~> ("(" ~> integerLit <~ ")").? ^^ { L4_NeumannBC(_) }
      ||| "None" ^^ { _ => L4_NoBC }
      ||| binaryexpression ^^ (L4_DirichletBC(_))
    )

  lazy val fieldCombinationDeclaration = locationize(("FieldCombination".? ~> ident) ~ levelDecl.? ~ (":" ~> stringLit) ~ ("=" ~> repsep(genericAccess, ","))
    ^^ { case id ~ levels ~ combType ~ fields => L4_FieldCombinationDecl(id, levels, combType, fields) })

  lazy val rangeIndex1d = locationize(("[" ~> binaryexpression.? <~ ":") ~ (binaryexpression.? <~ "]") ^^ { case x ~ y => L4_RangeIndex(L4_Range(x, y)) })
  lazy val rangeIndex2d = locationize("[" ~> binaryexpression.? ~ ":" ~ binaryexpression.? ~ "," ~ binaryexpression.? ~ ":" ~ binaryexpression.? <~ "]" ^^ {
    case a ~ _ ~ b ~ _ ~ x ~ _ ~ y => L4_RangeIndex(Array(L4_Range(a, b), L4_Range(x, y)))
  })

  lazy val stencilField = locationize((("StencilField" ~> ident) ~ ("<" ~> ident <~ "=>") ~ (ident <~ ">") ~ levelDecl.?)
    ^^ { case id ~ f ~ s ~ level => L4_StencilFieldDecl(id, level, s, f) })

  // ######################################
  // ##### "External" Definitions
  // ######################################

  lazy val externalField = locationize((("external" ~ "Field") ~> ident) ~ ("<" ~> ident <~ ">") ~ ("=>" ~> genericAccess)
    ^^ { case extid ~ layout ~ field => L4_ExternalFieldDecl(extid, layout, field) })

  // ######################################
  // ##### Object Access
  // ######################################

  lazy val slotAccess = locationize(
    "$" ~> slotModifier ^^ (s => s)
      ||| "[" ~> slotModifier <~ "]" ^^ (s => s))

  lazy val slotModifier = locationize("active" ^^ (_ => L4_ActiveSlot)
    ||| "activeSlot" ^^ (_ => L4_ActiveSlot)
    ||| "currentSlot" ^^ (_ => L4_ActiveSlot)
    ||| "next" ^^ (_ => L4_NextSlot)
    ||| "nextSlot" ^^ (_ => L4_NextSlot)
    ||| "previous" ^^ (_ => L4_PreviousSlot)
    ||| "previousSlot" ^^ (_ => L4_PreviousSlot)
    ||| integerLit ^^ (i => L4_ConstantSlot(i)))

  lazy val advanceStatement = locationize("advance" ~> genericAccess ^^ (a => L4_AdvanceSlot(a)))

  lazy val flatAccess = locationize(ident
    ^^ (id => L4_UnresolvedAccess(id)))
  lazy val leveledAccess = locationize(ident ~ levelAccess
    ^^ { case id ~ level => L4_UnresolvedAccess(id, Some(level)) })

  lazy val genericAccess = (
    locationize(ident ~ slotAccess.? ~ levelAccess.? ~ ("@" ~> constIndex).? ~ ("[" ~> integerLit <~ "]").?
      ^^ { case id ~ slot ~ level ~ offset ~ arrayIndex => L4_UnresolvedAccess(id, level, slot, offset, None, arrayIndex) })
      ||| locationize(ident ~ slotAccess.? ~ levelAccess.? ~ ("@" ~> constIndex).? ~ (":" ~> constIndex).?
      ^^ { case id ~ slot ~ level ~ offset ~ dirAccess => L4_UnresolvedAccess(id, level, slot, offset, dirAccess, None) })) // component acccess mit spitzen klammern

  // ######################################
  // ##### Expressions
  // ######################################

  lazy val binaryexpression : PackratParser[L4_Expression] = (
    locationize((binaryexpression ~ ("+" ||| "-") ~ term) ^^ { case lhs ~ op ~ rhs => L4_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term)

  lazy val term : PackratParser[L4_Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "%" ||| ".*" ||| "./" ||| ".%") ~ term2) ^^ { case lhs ~ op ~ rhs => L4_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term2)

  lazy val term2 : PackratParser[L4_Expression] = (
    locationize((term2 ~ ("**" ||| "^" ||| ".**") ~ factor) ^^ { case lhs ~ op ~ rhs => L4_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| factor)

  lazy val factor = (
    "(" ~> binaryexpression <~ ")"
      ||| ("-" ~ "(") ~> binaryexpression <~ ")" ^^ { L4_Negative }
      ||| rowVectorExpression
      ||| columnVectorExpression
      ||| matrixExpression
      ||| locationize("-" ~> matrixExpression ^^ { L4_Negative })
      ||| locationize(stringLit ^^ (s => L4_StringConstant(s)))
      ||| fieldIteratorAccess
      ||| locationize(booleanLit ^^ (s => L4_BooleanConstant(s)))
      ||| numLit
      ||| locationize("-" ~> functionCall ^^ { L4_Negative(_) })
      ||| functionCall
      ||| locationize("-" ~> genericAccess ^^ { L4_Negative(_) })
      ||| genericAccess
      ||| complexExpression
    //||| complexExpression2
    )

  lazy val numLit = locationize("-".? ~ numericLit ^^ { case s ~ n => if (isInt(s.getOrElse("") + n)) L4_IntegerConstant((s.getOrElse("") + n).toInt) else L4_RealConstant((s.getOrElse("") + n).toDouble) })

  lazy val rowVectorExpression = locationize("{" ~> repsep(binaryexpression, ",") <~ "}" ^^ { x => L4_VectorExpression(None, x, true) }
    ||| "[" ~> binaryexpression.+ <~ "]" ^^ { x => L4_VectorExpression(None, x, true) }) // Alternative version

  lazy val columnVectorExpression = locationize(rowVectorExpression <~ "T" ^^ (x => L4_VectorExpression(None, x.expressions, false)) |||
    "[" ~> repsep(binaryexpression, ";") <~ "]" ^^ { x => L4_VectorExpression(None, x, false) })

  lazy val matrixExpression = locationize(
    ("{" ~> repsep(rowVectorExpression, ",") <~ "}") ~ "T".? ^^ { case x ~ t => val e = L4_MatrixExpression(None, x.map(_.expressions.toList)); if (t.isDefined) L4_FunctionCall(L4_UnresolvedFunctionReference("transpose", None, None), e); else e } |||
      ("[" ~> repsep(binaryexpression.+, ";")) <~ "]" ^^ { x => L4_MatrixExpression(None, x) })

  lazy val complexExpression = locationize(
    /*
    ("(" ~> term) ~ "+" ~ term <~ ("j" ~ ")")
    ^^ { case real ~ _ ~ imag => L4_ComplexExpression(real, true, imag) } |||
    ("(" ~> term) ~ "-" ~ term <~ ("j" ~ ")")
      ^^ { case real ~ _ ~ imag => L4_ComplexExpression(real, false, imag) } |||

     */
    ("(" ~> term) ~ "+" ~ term <~ ("j" ~ ")")
      ^^ { case real ~ _ ~ imag => L4_ComplexExpression(real, true, imag) } |||
      ("(" ~> term) ~ "-" ~ term <~ ("j" ~ ")")
        ^^ { case real ~ _ ~ imag => L4_ComplexExpression(real, false, imag) } |||

      ("complex" ~ "(") ~> term ~ "+," ~ "-".? ~ term <~ ")"
    ^^ { case real ~ _ ~ sign ~ imag => L4_ComplexExpression(real, if(sign.isDefined) false else true, imag) })

  lazy val booleanexpression : PackratParser[L4_Expression] = (
    locationize((booleanexpression ~ ("||" ||| "or") ~ booleanexpression1) ^^ { case ex1 ~ op ~ ex2 => L4_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression1)

  lazy val booleanexpression1 : PackratParser[L4_Expression] = (
    locationize((booleanexpression1 ~ ("&&" ||| "and") ~ booleanexpression2) ^^ { case ex1 ~ op ~ ex2 => L4_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression2)

  lazy val booleanexpression2 : PackratParser[L4_Expression] = (
    locationize(("!" ~> booleanexpression3) ^^ (ex => L4_UnaryOperators.createExpression("!", ex)))
      ||| booleanexpression3)

  lazy val booleanexpression3 : PackratParser[L4_Expression] = (
    "(" ~> booleanexpression <~ ")"
      ||| comparison
      ||| binaryexpression)

  lazy val comparison : PackratParser[L4_Expression] = //(
    locationize((binaryexpression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ binaryexpression) ^^ { case ex1 ~ op ~ ex2 => L4_BinaryOperators.createExpression(op, ex1, ex2) })

  // #############################################################################
  // #################################### BASE ###################################
  // #############################################################################

  // ######################################
  // ##### L4_Index
  // ######################################

  lazy val index = expressionIndex ||| constIndex

  lazy val expressionIndex = locationize("[" ~> repsep(binaryexpression, ",") <~ "]" ^^ (l => L4_ExpressionIndex(l.toArray)))
  lazy val constIndex = (
    locationize("[" ~> repsep(integerLit, ",") <~ "]" ^^ (l => L4_ConstIndex(l.toArray)))
      ||| ("center" ||| "east" ||| "west" ||| "north" ||| "south" ||| "top" ||| "bottom") ^^ (s => L4_OffsetAlias.toConstIndex(s)))

  // #############################################################################
  // ################################## BASE_EXT #################################
  // #############################################################################

  // ######################################
  // ##### L4_ExpressionDeclaration
  // ######################################

  lazy val expressionDeclaration = locationize((("Expr" ||| "Expression") ~> ident) ~ levelDecl.? ~ ("=" ~> (binaryexpression ||| booleanexpression
    //||| complexExpression
    ))
    ^^ { case id ~ levels ~ exp => L4_ExpressionDeclaration(id, levels, exp) })

  // ######################################
  // ##### L4_FieldIteratorAccess
  // ######################################

  lazy val fieldIteratorAccess = (
    locationize(("i0" | "i1" | "i2") ^^ { id => L4_FieldIteratorAccess(id) })
      ||| locationize(("x" | "y" | "z") ^^ { id => L4_FieldIteratorAccess(id) })
    )

  // #############################################################################
  // #################################### FIELD ##################################
  // #############################################################################

  // ######################################
  // ##### L4_FieldDecl
  // ######################################

  lazy val localization = ("Node" ||| "node" ||| "Cell" ||| "cell"
    ||| "Face_x" ||| "face_x" ||| "Face_y" ||| "face_y" ||| "Face_z" ||| "face_z"
    ||| "Edge_Node" ||| "edge_node" ||| "Edge_Cell" ||| "edge_cell"
    ^^ (l => l))

  // #############################################################################
  // ################################# KNOWLEDGE #################################
  // #############################################################################

  // ######################################
  // ##### L4_InlineKnowledge
  // ######################################

  lazy val knowledgeParameter = locationize((ident <~ "=") ~ literal ^^ { case param ~ value => L4_KnowledgeParameter(param, value) })
  lazy val inlineKnowledge = locationize(("Knowledge" ~ "{") ~> knowledgeParameter.* <~ "}" ^^ (L4_InlineKnowledge(_)))

  // #############################################################################
  // ################################## OPERATOR #################################
  // #############################################################################

  // ######################################
  // ##### L4_StencilDecl
  // ######################################

  lazy val stencilDeclaration = (
    locationize(("Stencil" ~> ident) ~ levelDecl.? ~ ("{" ~> stencilEntries <~ "}")
      ^^ { case id ~ levels ~ entries => L4_BaseStencilDecl(id, levels, entries) })
      ||| locationize(("Stencil" ~> ident) ~ levelDecl.? ~ ("from" ~> binaryexpression)
      ^^ { case id ~ levels ~ expr => L4_StencilFromExpression(id, levels, expr) }))

  lazy val stencilEntries = (
    (stencilEntry <~ ",").* ~ stencilEntry ^^ { case entries ~ entry => entries.::(entry) }
      ||| stencilEntry.*)

  lazy val stencilEntry = (
    locationize((constIndex ~ ("=>" ~> binaryexpression)) ^^ { case offset ~ coeff => L4_StencilOffsetEntry(offset, coeff) })
      ||| locationize(((expressionIndex <~ "from") ~ expressionIndex ~ ("with" ~> binaryexpression)) ^^ { case row ~ col ~ coeff => L4_StencilMappingEntry(row, col, coeff) }))

  lazy val stencilFromDefault = (
    locationize(("Stencil" ~> ident) ~ levelDecl.? ~ (("from" ~ "default" ~ "restriction" ~ "on") ~> localization) ~ ("with" ~> stringLit)
      ^^ { case id ~ level ~ local ~ interpolation => L4_DefaultRestrictionStencil(id, level, local, interpolation) })
      ||| locationize(("Stencil" ~> ident) ~ levelDecl.? ~ (("from" ~ "default" ~ "prolongation" ~ "on") ~> localization) ~ ("with" ~> stringLit)
      ^^ { case id ~ level ~ local ~ interpolation => L4_DefaultProlongationStencil(id, level, local, interpolation) }))

  // #############################################################################
  // ################################### SOLVER ##################################
  // #############################################################################

  /// L4_EquationDecl

  lazy val equationExpression = locationize((binaryexpression <~ "==") ~ binaryexpression ^^ { case lhs ~ rhs => L4_Equation(lhs, rhs) })
  lazy val equationDeclaration = locationize(("Equation" ~> ident) ~ levelDecl.? ~ ("{" ~> equationExpression <~ "}")
    ^^ { case id ~ levels ~ eq => L4_EquationDecl(id, levels, eq) })
}
