package exastencils.parsers.settings

import exastencils.parsers.ExaParser
import scala.util.parsing.combinator._
import exastencils.core.UniversalSetter

class ParserSettings extends ExaParser {
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

  lazy val settingsfile = setting.*

  lazy val setting = ident ~ "=" ~ expr ^^ { case id ~ "=" ~ ex => UniversalSetter.withConversion(exastencils.core.Settings, id, ex) }

  lazy val expr = stringLit ^^ { _.toString } |||
    numericLit |||
    booleanLit ^^ { _.toBoolean }
}