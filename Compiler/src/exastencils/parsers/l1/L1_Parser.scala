package exastencils.parsers.l1

import scala.collection.immutable.PagedSeq
import scala.collection.mutable._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import exastencils.base.l1.L1_Root
import exastencils.parsers._

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

  lazy val program = (
    import_
    ).* ^^ { L1_Root(_) }

  lazy val import_ = "import" ~> stringLit ^^ { parseFile }

  //###########################################################

}
