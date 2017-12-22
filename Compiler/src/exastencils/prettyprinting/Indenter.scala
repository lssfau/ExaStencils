package exastencils.prettyprinting

import scala.collection.mutable.StringBuilder

object Indenter {
  def addIndentations(toIndent : String) : String = {
    var indent = 0
    var output : StringBuilder = new StringBuilder

    for (c <- toIndent) {
      c match {
        case '{'  =>
          indent += 1
          output.append(c)
        case '}'  =>
          indent -= 1
          if ('\t' == output.charAt(output.length - 1))
            output.deleteCharAt(output.length - 1) // remove last tab
          output.append(c)
        case '\n' =>
          output.append(c)
          for (_ <- 0 until indent)
            output.append('\t')
        case _    =>
          output.append(c)
      }
    }

    output.toString
  }
}