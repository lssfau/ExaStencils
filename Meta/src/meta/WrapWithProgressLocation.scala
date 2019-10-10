package meta

import scala.collection.JavaConverters._

import java.util.stream.Collectors

object WrapWithProgressLocation {
  def apply(input : String) : String = {
    // check if early exit is possible
    if (!input.contains("def progress"))
      return input

    var processed = ""

    var closeNextLine = false
    input.lines.collect(Collectors.toList()).asScala.foreach {
      case s if closeNextLine =>
        closeNextLine = false
        processed += s + '\n'
        processed += "\t}\n"

      case s if s.contains("def progress ") && s.contains("=") && !s.contains("ProgressLocation") =>
        var ss = s

        // peel indentation
        while (ss.nonEmpty && (' ' == ss(0) || '\t' == ss(0))) {
          processed += ss(0)
          ss = ss.drop(1)
        }

        // add missing override keywords
        if (!ss.startsWith("override"))
          ss = "override " + ss

        // peel override def
        if (!ss.startsWith("override def progress ")) {
          println(s"malformed progress definition in \n$s\n$ss")
          return input
        }
        ss = ss.drop("override def progress ".length)
        processed += "override def progress "

        // peel potential type specification
        val i = ss.indexOf('=')
        processed += ss.substring(0, i + 1)
        ss = ss.drop(i + 1)

        // peel more spaces
        while (ss.nonEmpty && (' ' == ss(0) || '\t' == ss(0))) {
          processed += ss(0)
          ss = ss.drop(1)
        }

        // check for different progress shapes
        if (ss.isEmpty) {
          // single line in next line
          processed += " ProgressLocation {"
          closeNextLine = true
        } else if (ss.startsWith("???") || ss.startsWith("Logger.error") || ss.startsWith("{ Logger.error")) {
          // no wrapping required
          processed += ss
        } else if ('{' == ss(0)) {
          // function body in curly braces
          processed += "ProgressLocation " + ss
        } else if (ss.contains("//")) {
          // single line with comment
          val ssSplit = ss.split("//")

          var body = ssSplit.head
          var comments = "//" + ssSplit.tail.mkString("//")

          // re-arrange spaces
          while (body.nonEmpty && (' ' == body.last || '\t' == body.last)) {
            comments = body.last + comments
            body = body.dropRight(1)
          }

          processed += "ProgressLocation(" + body + ")" + comments
        } else {
          // single line
          processed += "ProgressLocation(" + ss + ")"
        }

        processed += "\n"

      case s =>
        processed += s
        processed += "\n"
    }

    if (processed != input
      && (!(processed.contains("import exastencils.base.ProgressLocation\n") || processed.contains("import exastencils.base._\n")))) {
      processed = addImport(processed)
    }

    processed
  }

  def addImport(input : String) = {
    var output = ""

    var importAdded = false

    input.lines.collect(Collectors.toList()).asScala.foreach {
      case s if importAdded             => output += s + '\n'
      case ""                           => output += '\n'
      case s if s.startsWith("package") => output += s + '\n'

      case s if s.startsWith("import exastencils") =>
        if (s < "import exastencils.base.ProgressLocation") {
          output += s + '\n'
        } else {
          output += "import exastencils.base.ProgressLocation\n"
          output += s + '\n'
          importAdded = true
        }

      case s if s.startsWith("import") => output += s + '\n'

      case s => // anything else
        output += "import exastencils.base.ProgressLocation\n\n"
        output += s + '\n'
        importAdded = true
    }

    output
  }
}