package exastencils.util

import scala.util.parsing.json._
import exastencils.logger._

object InputReader {
  private var settings_ = ""
  private var knowledge_ = ""
  private var layer4_ = ""

  def settings = settings_
  def knowledge = knowledge_
  def layer4 = layer4_

  def read(filename : String) = {
    val lines = io.Source.fromFile(filename).getLines.mkString
    readJson(lines)
  }

  def read() = {
    val lines = io.Source.stdin.getLines.mkString
    readJson(lines)
  }

  def readJson(s : String) = {
    val result = JSON.parseFull(s)

    result match {
      case Some(e) =>
        e match {
          case m : Map[Any, Any] => extract(m)
          case _                 => Logger.error("Unexpected JSON structure")
        }
      case None => Logger.error("Could not parse JSON")
    }
  }

  private def extract(map : Map[Any, Any]) = {
    map.foreach(f => {
      f._1 match {
        case "settings"  => settings_ = f._2.asInstanceOf[String]
        case "knowledge" => knowledge_ = f._2.asInstanceOf[String]
        case "layer1"    =>
        case "layer2"    =>
        case "layer3"    =>
        case "layer4"    => layer4_ = f._2.asInstanceOf[String]
        case "tpdl"      =>
        case _           => Logger.error("Unexpected file identifier in JSON")
      }
    })
  }
}
