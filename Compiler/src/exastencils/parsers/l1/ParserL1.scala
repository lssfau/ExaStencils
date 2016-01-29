package exastencils.parsers.l1

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader

import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.parsers._

class ParserL1 extends ExaParser with scala.util.parsing.combinator.PackratParsers {
  override val lexical : ExaLexer = new LexerL1()

  def parse(s : String) : Node = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : Node = {
    val lines = io.Source.fromFile(filename).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    parseTokens(scanner)
  }

  protected def parseTokens(tokens : lexical.Scanner) : Node = {
    phrase(program)(tokens) match {
      case Success(e, _) => e
      case Error(msg, _) => throw new Exception("parse error: " + msg)
      case Failure(msg, parser) => {
        var sb = new StringBuilder
        sb.append(s"Parse failure at position ${parser.pos}: $msg\n")
        sb.append(parser.pos.longString)
        sb.append("\n")
        throw new Exception(sb.toString)
      }
    }
  }

  //###########################################################

  lazy val program = ???

}
