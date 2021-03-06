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

package exastencils.parsers.config

import scala.collection.mutable._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input._

import exastencils.parsers._
import exastencils.runner._

/// Samples_Parser

class Runner_Parser extends ExaParser {
  override val lexical : StdLexical = new Runner_Lexer()

  def parse(s : String) : Unit = {
    parseTokens(new lexical.Scanner(s))
  }

  private val prevDirs = new Stack[java.io.File]().push(null)
  def parseFile(filename : String) : RunnerConfig = {
    val file = new java.io.File(prevDirs.top, filename)
    val lines = io.Source.fromFile(file).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    prevDirs.push(file.getAbsoluteFile.getParentFile)
    val ret = parseTokens(scanner)
    prevDirs.pop()
    ret
  }

  protected def parseTokens(tokens : lexical.Scanner) : RunnerConfig = {
    phrase(samplesfile)(tokens) match {
      case Success(e, _)   => e
      case Error(msg, _)   => throw new Exception("parse error: " + msg)
      case Failure(msg, _) => throw new Exception("parse failure: " + msg)
    }
  }

  lazy val samplesfile = (
    (("Variabilities" ~ "{") ~> sample.* <~ "}").?
      ~ (("Constraints" ~ "{") ~> constraint.* <~ "}").?
      ~ (("DerivedParameters" ~ "{") ~> derived.* <~ "}").?
      ^^ { case samples ~ constraints ~ derived => RunnerConfig(samples, constraints, derived) })

  lazy val sample = (
    (ident <~ "=") ~ ("{" ~> expressionList <~ "}") ^^ { case name ~ values => VariabilitiesFromList(name, values) }
      ||| (ident <~ "=") ~ code ^^ { case name ~ c => VariabilitiesFromLambda(name, c) }
    )
  lazy val constraint = code ^^ Constraint
  lazy val derived = (
    (ident <~ "=") ~ expr ^^ { case name ~ value => DerivedParameterWithAssign(name, value) }
      ||| (ident <~ "+=") ~ expr ^^ { case name ~ value => DerivedParameterWithAppend(name, value) }
    )

  lazy val expressionList = /*locationize*/ repsep(expr, ",") ^^ (args => args)

  lazy val expr = (
    stringLit ^^ { _.toString }
      ||| "-".? ~ numericLit ^^ {
      case s ~ n if isInt(s.getOrElse("") + n) => (s.getOrElse("") + n).toInt : AnyVal
      case s ~ n                               => (s.getOrElse("") + n).toDouble : AnyVal
    }
      ||| booleanLit ^^ { _.booleanValue() }
      ||| code
    )

  lazy val code = "~" ~> stringLit <~ "~" ^^ (s => CodeWrapper(s.toString))
}
