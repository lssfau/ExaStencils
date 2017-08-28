package exastencils.parsers.config

import scala.collection.immutable.PagedSeq
import scala.collection.mutable.Stack
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.PagedSeqReader

import exastencils.config.Knowledge
import exastencils.core._
import exastencils.logger._
import exastencils.parsers._

/// Knowledge_Parser

class Knowledge_Parser extends ExaParser {
  override val lexical : StdLexical = new Knowledge_Lexer()

  def parse(s : String) : Unit = {
    parseTokens(new lexical.Scanner(s))
  }

  private val prevDirs = new Stack[java.io.File]().push(null)
  def parseFile(filename : String) : Unit = {
    val file = new java.io.File(prevDirs.top, filename)
    val lines = io.Source.fromFile(file).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    prevDirs.push(file.getAbsoluteFile.getParentFile)
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
      UniversalSetter(Knowledge, ident, value)
    } catch {
      case ex : java.lang.NoSuchFieldException     => Logger.warning(s"Trying to set parameter Knowledge.${ ident } to ${ value } but this parameter is undefined")
      case ex : java.lang.IllegalArgumentException => Logger.error(s"Trying to set parameter Knowledge.${ ident } to ${ value } but data types are incompatible")
    }
  }

  lazy val settingsfile = setting.*

  lazy val setting = ("import" ~> stringLit ^^ (path => parseFile(path))
    ||| ident ~ "=" ~ literal ^^ { case id ~ "=" ~ ex => setParameter(id, ex) })
}
