import exastencils.base.ExaRootNode
import exastencils.base.l4._
import exastencils.config._
import exastencils.parsers.config._
import exastencils.parsers.l4._

object ValidateL4 {
  def main(args : Array[String]) : Unit = {
    // check from where to read input
    val settingsParser = new Settings_Parser(Settings)
    val knowledgeParser = new Settings_Parser(Knowledge)
    if (args.length >= 1) {
      settingsParser.parseFile(args(0))
    }
    if (args.length >= 2) {
      knowledgeParser.parseFile(args(1))
    }
    Knowledge.update()

    ExaRootNode.l4_root = L4_Root(Settings.getL4file.map(L4_Parser.parseFile(_) : L4_Node))
    ExaRootNode.l4_root.flatten()
    L4_Validation.apply()
  }
}
