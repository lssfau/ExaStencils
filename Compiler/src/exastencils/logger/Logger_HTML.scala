package exastencils.logger

import exastencils.core._

object Logger_HTML {
  var isInit : Boolean = false
  var log : java.io.FileWriter = null

  def init = {
    val targetFile = Settings.getHtmlLogFile
    if (!(new java.io.File(targetFile)).exists) {
      var file = new java.io.File(targetFile)
      if (!file.getParentFile().exists()) file.getParentFile().mkdirs()
    }

    log = new java.io.FileWriter(targetFile)

    log.write("<HEAD><TITLE>ExaStencils Log</TITLE></HEAD>\n")
    log.write("<script type=\"text/JavaScript\">\n")
    //log.write("function AutoRefresh (t) { setTimeout(\"location.reload(true);\", t); }\n")
    //log.write("function AutoScroll (t) { window.scrollTo(0, document.body.scrollHeight); setTimeout(\"AutoScroll()\", t); }\n"
    log.write("</script>\n")
    log.write("<body bgcolor=\"black\" onload=\"JavaScript:AutoRefresh(5000);JavaScript:AutoScroll(1000);\">")
    log.write("<table border=\"1\">\n")
    log.write("<tr><td><span style=\"color:white\">File</span></td>"
      + "<td><span style=\"color:white\">Line</span></td>"
      + "<td><span style=\"color:white\">Message</span></td>"
      + "<td><span style=\"color:white\">Level</span></td></tr>\n")

    isInit = true
  }

  def finish = {
    log.write("</table></body>")
    log.close

    isInit = false
  }

  def printInfo(fileName : String, line : Int, message : AnyRef) {
    log.write("<tr><td><span style=\"color:white\">" + fileName + "</span></td><td><span style=\"color:white\">" + line + "</span></td>")
    log.write("<td><span style=\"color:white\">" + message + "</span></td><td><span style=\"color:white\">Info</span></td></tr>\n")
  }

  def printDbg(fileName : String, line : Int, message : AnyRef) {
    log.write("<tr><td><span style=\"color:white\">" + fileName + "</span></td><td><span style=\"color:white\">" + line + "</span></td>")
    log.write("<td><span style=\"color:green\">" + message + "</span></td><td><span style=\"color:white\">Debug</span></td></tr>\n")
  }

  def printWarn(fileName : String, line : Int, message : AnyRef) {
    log.write("<tr><td><span style=\"color:white\">" + fileName + "</span></td><td><span style=\"color:white\">" + line + "</span></td>")
    log.write("<td><span style=\"color:yellow\">" + message + "</span></td><td><span style=\"color:white\">Warning</span></td></tr>\n")
  }

  def printErr(fileName : String, line : Int, message : AnyRef) {
    log.write("<tr><td><span style=\"color:white\">" + fileName + "</span></td><td><span style=\"color:white\">" + line + "</span></td>")
    log.write("<td><span style=\"color:red\">" + message + "</span></td><td><span style=\"color:white\">Error</span></td></tr>\n")
  }
}
