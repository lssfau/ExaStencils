package exastencils.parsers.settings

import scala.collection.immutable.PagedSeq
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.PagedSeqReader

import exastencils.core._
import exastencils.logger._
import exastencils.parsers._

class ParserSettings extends ExaParser {
  override val lexical : StdLexical = new LexerSettings()

  def parse(s : String) : Unit = {
    parseTokens(new lexical.Scanner(s))
  }

  private val prevDirs = new Stack[java.io.File]().push(null)
  def parseFile(filename : String) : Unit = {
    val file = new java.io.File(prevDirs.top, filename)
    val lines = io.Source.fromFile(file).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    prevDirs.push(file.getAbsoluteFile().getParentFile())
    parseTokens(scanner)
    prevDirs.pop()
  }

  protected def parseTokens(tokens : lexical.Scanner) : Unit = {
    phrase(settingsfile)(tokens) match {
      case Success(e, _)   =>
      case Error(msg, _)   => throw new Exception("parse error: " + msg)
      case Failure(msg, _) => throw new Exception("parse failure: " + msg)
    }
  }

  def setParameter[T](ident : String, value : T) = {
    try {
      UniversalSetter(exastencils.core.Settings, ident, value)
    } catch {
      case ex : java.lang.NoSuchFieldException     => Logger.warning(s"Trying to set parameter Settings.${ident} to ${value} but this parameter is undefined")
      case ex : java.lang.IllegalArgumentException => Logger.error(s"Trying to set parameter Settings.${ident} to ${value} but data types are incompatible")
    }
  }

  def addParameter[T](ident : String, value : T) = {
    try {
      UniversalSetter.addToListBuffer(exastencils.core.Settings, ident, value)
    } catch {
      case ex : java.lang.NoSuchFieldException     => Logger.warning(s"Trying to set parameter Settings.${ident} to ${value} but this parameter is undefined")
      case ex : java.lang.IllegalArgumentException => Logger.error(s"Trying to set parameter Settings.${ident} to ${value} but data types are incompatible")
    }
  }

  lazy val settingsfile = setting.*

  lazy val expressionList = /*locationize*/ ((expr <~ ("," | newline)).* ~ expr ^^ { case args ~ arg => args :+ arg })

  lazy val setting = ("import" ~> stringLit ^^ { case path => parseFile(path) }
    ||| (ident <~ "=") ~ expr ^^ { case id ~ ex => setParameter(id, ex) }
    ||| (ident <~ "+=") ~ expr ^^ { case id ~ ex => addParameter(id, ex) }
    ||| (ident <~ "+=") ~ ("{" ~> expressionList <~ "}") ^^ { case id ~ exs => for (ex <- exs) addParameter(id, ex) }
    ||| (ident <~ "=") ~ ("{" ~> expressionList <~ "}") ^^ { case id ~ exs => setParameter(id, exs.to[ListBuffer]) })

  lazy val expr = stringLit ^^ { _.toString } |
    "-".? ~ numericLit ^^ {
      case s ~ n if (isInt(s.getOrElse("") + n)) => (s.getOrElse("") + n).toInt : AnyVal
      case s ~ n                                 => (s.getOrElse("") + n).toDouble : AnyVal
    } |
    booleanLit ^^ { _.booleanValue() }
}
