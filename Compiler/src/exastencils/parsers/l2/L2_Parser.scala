package exastencils.parsers.l2

import scala.collection.immutable.PagedSeq
import scala.collection.mutable._
import scala.io._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import exastencils.base.l2.L2_Root
import exastencils.domain.l2.L2_DomainDecl
import exastencils.parsers._

object L2_Parser extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = L2_Lexer

  def parse(s : String) : L2_Root = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : L2_Root = {
    val lines = Source.fromFile(filename).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    parseTokens(scanner)
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
    domainDeclaration
    ).* ^^ { nodes => L2_Root(nodes) }

  //###########################################################

  // #############################################################################
  // #################################### BASE ###################################
  // #############################################################################

  // #############################################################################
  // ################################## BASE_EXT #################################
  // #############################################################################

  // #############################################################################
  // ################################### DOMAIN ##################################
  // #############################################################################

  // ######################################
  // ##### L2_DomainDecl
  // ######################################

  lazy val domainDeclaration = locationize("Domain" ~> ident ^^ { L2_DomainDecl })

}
