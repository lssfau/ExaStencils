package exastencils.core.logger

import exastencils.config.Settings

object Logger_HTML {
  var isInit : Boolean = false
  var log : java.io.FileWriter = null

  def init() = {
    val targetFile = Settings.getHtmlLogFile
    if (!new java.io.File(targetFile).exists) {
      val file = new java.io.File(targetFile)
      if (!file.getParentFile.exists()) file.getParentFile.mkdirs()
    }

    log = new java.io.FileWriter(targetFile)

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

  def finish() = {
    if (isInit) {
      this <<< "</table></body>"
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
    log.write(htmlfy(msg))
    this
  }
  def <<<(msg : AnyRef) : this.type = {
    log.write(htmlfy(msg) + '\n')
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
