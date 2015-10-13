package exastencils.parsers.settings

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader

import exastencils.core._
import exastencils.logger._
import exastencils.parsers._

class ParserKnowledge extends ExaParser {
  def parse(s : String) : Unit = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : Unit = {
    import scala.util.parsing.input._
    import scala.collection.immutable.PagedSeq

    val lines = io.Source.fromFile(filename).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    parseTokens(scanner)
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
      UniversalSetter(exastencils.knowledge.Knowledge, ident, value)
    } catch {
      case ex : java.lang.NoSuchFieldException     => Logger.warning(s"Trying to set parameter Knowledge.${ident} to ${value} but this parameter is undefined")
      case ex : java.lang.IllegalArgumentException => Logger.error(s"Trying to set parameter Knowledge.${ident} to ${value} but data types are incompatible")
    }
  }

  lazy val settingsfile = setting.*

  lazy val setting = ident ~ "=" ~ expr ^^ { case id ~ "=" ~ ex => setParameter(id, ex) }

  lazy val expr = stringLit ^^ { _.toString } |
    "-".? ~ numericLit ^^ {
      case s ~ n if (isInt(s.getOrElse("") + n)) => (s.getOrElse("") + n).toInt : AnyVal
      case s ~ n                                 => (s.getOrElse("") + n).toDouble : AnyVal
    } |
    booleanLit ^^ { _.booleanValue() }
}
