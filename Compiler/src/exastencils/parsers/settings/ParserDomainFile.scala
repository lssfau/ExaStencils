package exastencils.parsers.settings

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import exastencils.core._
import exastencils.logger._
import exastencils.parsers._
import exastencils.domain._
import scala.collection.mutable.ListBuffer
import exastencils.knowledge.Knowledge

class ParserDomainFile extends ExaParser {

  def parseHeader(filename : String) : Unit = {
    val file = io.Source.fromFile(filename)
    val lines = file.getLines.dropWhile { s => s != "DATA" }.drop(1)
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines.takeWhile { s => s != "DOMAINS" }))
    val scanner = new lexical.Scanner(reader)

    parseHeaderTokens(scanner)
    file.close()
  }

  protected def parseHeaderTokens(tokens : lexical.Scanner) : Unit = {
    phrase(headerSettings)(tokens) match {
      case Success(e, _)   =>
      case Error(msg, _)   => throw new Exception("parse error: " + msg)
      case Failure(msg, _) => throw new Exception("parse failure: " + msg)
    }
  }

  def setHeaderParameter[T](ident : String, value : T) = {
    try {
      UniversalSetter(exastencils.domain.DomainFileHeader, ident, value)
    } catch {
      case ex : java.lang.NoSuchFieldException     => Logger.warning(s"Trying to set parameter DomainFile.${ident} to ${value} but this parameter is undefined")
      case ex : java.lang.IllegalArgumentException => Logger.error(s"Trying to set parameter DomainFile.${ident} to ${value} but data types are incompatible")
      case e : Exception                           => println("Error " + e)
    }
  }

  lazy val headerSettings = header.*
  lazy val header = stringLit ||| ident ~ "=" ~ headerExpr ^^ { case id ~ "=" ~ ex => setHeaderParameter(id, ex) }
  lazy val headerExpr =
    arrayLit |
      stringLit ^^ { _.toString().split(",") } |
      "-".? ~ numericLit ^^ {
        case s ~ n if (isInt(s.getOrElse("") + n)) => (s.getOrElse("") + n).toInt : AnyVal
        case s ~ n                                 => (s.getOrElse("") + n).toDouble : AnyVal
      } |
      booleanLit ^^ { _.toBoolean }

  def parseBody(filename : String) : Unit = {
    val file = io.Source.fromFile(filename)
    val lines = file.getLines.dropWhile { s => s != "DOMAINS" }.drop(1)
    val readerDomains = new PagedSeqReader(PagedSeq.fromLines(lines.takeWhile { s => s != "BLOCKS" }))
    val scannerDomains = new lexical.Scanner(readerDomains)
    parseDomainTokens(scannerDomains)

    val readerBlocks = new PagedSeqReader(PagedSeq.fromLines(lines.takeWhile { s => s != "FRAGMENTS" }))
    val scannerBlocks = new lexical.Scanner(readerBlocks)
    parseBlockTokens(scannerBlocks)

    val readerFragments = new PagedSeqReader(PagedSeq.fromLines(lines.takeWhile { s => s != "TRAFOS" }))
    val scannerFragments = new lexical.Scanner(readerFragments)
    parseFragmentTokens(scannerFragments)

    val readerTrafos = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scannerTrafos = new lexical.Scanner(readerTrafos)
    parseTrafoTokens(scannerTrafos)

    file.close()
  }

  protected def parseDomainTokens(tokens : lexical.Scanner) : Unit = {
    phrase(domainSettings)(tokens) match {
      case Success(e, _)   =>
      case Error(msg, _)   => throw new Exception("parse error: " + msg)
      case Failure(msg, t) => throw new Exception("parse failure: " + msg + tmpDomains)
    }
  }

  protected def parseBlockTokens(tokens : lexical.Scanner) : Unit = {
    phrase(blockSettings)(tokens) match {
      case Success(e, _)   =>
      case Error(msg, _)   => throw new Exception("parse error: " + msg)
      case Failure(msg, t) => throw new Exception("parse failure: " + msg + tmpBlocks)
    }
  }

  protected def parseFragmentTokens(tokens : lexical.Scanner) : Unit = {
    phrase(fragmentSettings)(tokens) match {
      case Success(e, _)   =>
      case Error(msg, _)   => throw new Exception("parse error: " + msg)
      case Failure(msg, t) => throw new Exception("parse failure: " + msg)
    }
  }

  protected def parseTrafoTokens(tokens : lexical.Scanner) : Unit = {
    phrase(trafoSettings)(tokens) match {
      case Success(e, _)   =>
      case Error(msg, _)   => throw new Exception("parse error: " + msg)
      case Failure(msg, t) => throw new Exception("parse failure: " + msg)
    }
  }

  var tmpDomains : Map[String, Any] = Map()
  var tmpBlocks : Map[String, Any] = Map()

  def setDomainParameter[T](ident : String, value : T) = {
    exastencils.knowledge.DomainCollection.getDomainByIdentifier("global").get
      .asInstanceOf[exastencils.knowledge.FileInputGlobalDomain].shape.asInstanceOf[List[exastencils.knowledge.FileInputDomain]]
      .find { d => d.identifier == ident } match {
        case Some(n) => n.shape.asInstanceOf[FileInputDomainShape].blocks = value.asInstanceOf[List[String]]
        case None    => throw new Exception("error when parsing domain file")
      }
    tmpDomains = tmpDomains + (ident -> value)
  }

  def setBlockParameter[T](ident : String, value : T) = {
    exastencils.knowledge.DomainCollection.getDomainByIdentifier("global").get
      .asInstanceOf[exastencils.knowledge.FileInputGlobalDomain].shape.asInstanceOf[List[exastencils.knowledge.FileInputDomain]]
      .find { d => d.shape.asInstanceOf[FileInputDomainShape].blocks.contains(ident) } match {
        case Some(n) => n.shape.asInstanceOf[FileInputDomainShape].frags ++= value.asInstanceOf[List[String]]
        case None    => throw new Exception("error when parsing domain file")
      }
    tmpBlocks = tmpBlocks + (ident -> value)
  }

  def setFragmentParameter[T](ident : String, value : T) = {
    val valueList = value.asInstanceOf[List[List[List[List[String]]]]]
    var vertices : ListBuffer[Vertex] = ListBuffer()
    var edges : ListBuffer[Edge] = ListBuffer()
    val faces = valueList.map { face =>
      val e = face.map {
        edge =>
          val v = edge.map {
            vertex =>
              new Vertex(vertex.map { x => x.toDouble }.to[ListBuffer])
          }
          vertices ++= v
          new Edge(v.head, v.last)
      }
      edges ++= e
      new Face(edges, vertices)
    }.to[ListBuffer]

    val localId = tmpBlocks.values.toList.map(f => f.asInstanceOf[List[String]].zipWithIndex.find(f => f._1 == ident)).filter(f => f.isDefined).head.get._2
    val globalId = ident.drop(1).toInt
    val blockIdent = tmpBlocks.find(f => f._2.asInstanceOf[List[String]].contains(ident)).get._1
    val domainIds = exastencils.knowledge.DomainCollection.getDomainByIdentifier("global").get
      .asInstanceOf[exastencils.knowledge.FileInputGlobalDomain].shape.asInstanceOf[List[exastencils.knowledge.FileInputDomain]]
      .filter { domFil =>
        tmpDomains.filter(f => f._2.asInstanceOf[List[String]].contains(blockIdent)).contains(domFil.identifier)
      }
      .map(m => m.index).to[ListBuffer]
    FragmentCollection.fragments += new Fragment(localId, globalId, domainIds, faces, edges, vertices.toSeq.distinct.to[ListBuffer], ListBuffer.fill(FragmentCollection.getNumberOfNeighbors)(-1), blockIdent.drop(1).toInt)
  }

  def setTrafoParameter[T](ident : String, value : T) = {
    val trafoList = value.asInstanceOf[List[List[String]]]
    val trafos : ListBuffer[Double] = ListBuffer()
    FragmentCollection.fragments.find { f => f.globalId == ident.drop(1).toInt } match {
      case Some(f) => {
        val tmp = trafoList
        f.trafo = trafoList.map { row => row.map { item => item.toDouble } }.flatten.to[ListBuffer]
      }
      case None =>
    }
  }

  lazy val domainSettings = domain.*
  lazy val domain = ident ~ "=" ~ identArrayLit ^^ { case id ~ "=" ~ ex => setDomainParameter(id, ex) }

  lazy val blockSettings = block.*
  lazy val block = ident ~ "=" ~ identArrayLit ^^ { case id ~ "=" ~ ex => setBlockParameter(id, ex) }

  lazy val fragmentSettings = fragment.*
  lazy val fragment = ident ~ "=" ~ fragmentLit ^^ { case id ~ "=" ~ ex => setFragmentParameter(id, ex) }

  lazy val trafo = ident ~ "=" ~ trafoLit ^^ { case id ~ "=" ~ ex => setTrafoParameter(id, ex) }
  lazy val trafoSettings = trafo.*

  lazy val realNumLit = "-".? ~ numericLit ^^ {
    case s ~ n => s.getOrElse("") + n
  }
  lazy val arrayLit = "(" ~> repsep(realNumLit, ",") <~ ")"
  lazy val identArrayLit = "(" ~> repsep(ident, ",") <~ ")"
  lazy val vertexLit = "(" ~> repsep(numericLit, ",") <~ ")"
  lazy val edgeLit = "(" ~> repsep(vertexLit, ",") <~ ")"
  lazy val faceLit = "(" ~> repsep(edgeLit, ",") <~ ")"
  lazy val fragmentLit = "(" ~> repsep(faceLit, ",") <~ ")"
  lazy val trafoLit = "(" ~> repsep(arrayLit, ",") <~ ")"
}

