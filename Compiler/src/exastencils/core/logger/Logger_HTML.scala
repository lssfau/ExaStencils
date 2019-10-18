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

package exastencils.core.logger

import exastencils.config.Settings

object Logger_HTML {
  var isInit : Boolean = false
  var bufferMode : Boolean = true
  var buffer : java.io.StringWriter = null
  var log : java.io.FileWriter = null

  def init() = {
    buffer = new java.io.StringWriter()
    bufferMode = true

    this <<< "<HEAD><TITLE>ExaStencils Log</TITLE></HEAD>"
    //this <<< "<script type=\"text/JavaScript\">"
    //this <<< "function AutoRefresh (t) { setTimeout(\"location.reload(true);\", t); }"
    //this <<< "function AutoScroll (t) { window.scrollTo(0, document.body.scrollHeight); setTimeout(\"AutoScroll()\", t); }"
    //this <<< "</script>"
    //this << "<body bgcolor=\"black\" onload=\"JavaScript:AutoRefresh(5000);JavaScript:AutoScroll(1000);\">"
    this << "<body bgcolor=\"black\">"
    this <<< "<table border=\"1\">"
    this << "<tr><td><span style=\"color:white\">File</span></td>"
    this << "<td><span style=\"color:white\">Line</span></td>"
    this << "<td><span style=\"color:white\">Message</span></td>"
    this <<< "<td><span style=\"color:white\">Level</span></td></tr>"

    isInit = true
  }

  def beginFileWrite() = {
    val targetFile = Settings.getHtmlLogFile
    if (!new java.io.File(targetFile).exists) {
      val file = new java.io.File(targetFile)
      if (!file.getParentFile.exists()) file.getParentFile.mkdirs()
    }

    log = new java.io.FileWriter(targetFile)

    log.write(buffer.toString)

    bufferMode = false
  }

  def finish() = {
    if (isInit) {
      this <<< "</table></body>"

      if (bufferMode) beginFileWrite()

      log.close()

      isInit = false
    }
  }

  def htmlfy(msg : AnyRef) : String = {
    var out = ""
    var inTag = false

    for (c <- msg.toString) c match {
      case '<' => inTag = true; out += c
      case '>' => inTag = false; out += c

      //case ' ' if !inTag  => out += "&nbsp;"
      case '\n' if !inTag => out += "<br>"
      case '\t' if !inTag => out += "&nbsp;&nbsp;"

      case o => out += o
    }

    out
  }

  def <<(msg : AnyRef) : this.type = {
    if (bufferMode)
      buffer.write(htmlfy(msg))
    else
      log.write(htmlfy(msg))

    this
  }
  def <<<(msg : AnyRef) : this.type = {
    if (bufferMode)
      buffer.write(htmlfy(msg) + "\n")
    else
      log.write(htmlfy(msg) + "\n")

    this
  }

  def printInfo(fileName : String, line : Int, message : AnyRef) = {
    if (isInit) {
      this << "<tr><td><span style=\"color:white\">" << fileName << "</span></td><td><span style=\"color:white\">" << line.toString << "</span></td>"
      this << "<td><span style=\"color:white\">" << message <<< "</span></td><td><span style=\"color:white\">Info</span></td></tr>"
    }
  }

  def printDbg(fileName : String, line : Int, message : AnyRef) = {
    if (isInit) {
      this << "<tr><td><span style=\"color:white\">" << fileName << "</span></td><td><span style=\"color:white\">" << line.toString << "</span></td>"
      this << "<td><span style=\"color:green\">" << message <<< "</span></td><td><span style=\"color:white\">Debug</span></td></tr>"
    }
  }

  def printWarn(fileName : String, line : Int, message : AnyRef) = {
    if (isInit) {
      this << "<tr><td><span style=\"color:white\">" << fileName << "</span></td><td><span style=\"color:white\">" << line.toString << "</span></td>"
      this << "<td><span style=\"color:yellow\">" << message <<< "</span></td><td><span style=\"color:white\">Warning</span></td></tr>"
    }
  }

  def printErr(fileName : String, line : Int, message : AnyRef) = {
    if (isInit) {
      this << "<tr><td><span style=\"color:white\">" << fileName << "</span></td><td><span style=\"color:white\">" << line.toString << "</span></td>"
      this << "<td><span style=\"color:red\">" << message <<< "</span></td><td><span style=\"color:white\">Error</span></td></tr>"
    }
  }
}
