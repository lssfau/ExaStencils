import exastencils.base.ExaRootNode
import exastencils.config._
import exastencils.core.StateManager
import exastencils.parsers.InputReader
import exastencils.parsers.l4._

object ValidateL4 {
  def main(args : Array[String]) : Unit = {
    // check from where to read input
    val settingsParser = new exastencils.parsers.settings.ParserSettings
    val knowledgeParser = new exastencils.parsers.settings.ParserKnowledge
    if (args.length == 1 && args(0) == "--json-stdin") {
      InputReader.read
      settingsParser.parse(InputReader.settings)
      knowledgeParser.parse(InputReader.knowledge)
      Knowledge.l3tmp_generateL4 = false // No Layer4 generation with input via JSON
    } else if (args.length == 2 && args(0) == "--json-file") {
      InputReader.read(args(1))
      settingsParser.parse(InputReader.settings)
      knowledgeParser.parse(InputReader.knowledge)
      Knowledge.l3tmp_generateL4 = false // No Layer4 generation with input via JSON
    } else {
      if (args.length >= 1) {
        settingsParser.parseFile(args(0))
      }
      if (args.length >= 2) {
        knowledgeParser.parseFile(args(1))
      }
    }
    Knowledge.update()

    if (Settings.inputFromJson) {
      ExaRootNode.l4_root = (new ParserL4).parseFile(InputReader.layer4)
    } else {
      ExaRootNode.l4_root = (new ParserL4).parseFile(Settings.getL4file)
    }
    ValidationL4.apply
  }
}
