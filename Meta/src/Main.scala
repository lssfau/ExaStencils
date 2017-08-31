import meta._

object Main {
  def main(args : Array[String]) : Unit = {
    println(s"Performing operation '${ args(0) }'")

    args(0) match {
      case "collect"   => collect()
      case "generate"  => generate()
      case "duplicate" => duplicate()
      case other       => println(s"Unknown operation $other")
    }
  }

  def collect() = {
    for (entry <- MuncherList.entries)
      CodeMuncher.process(entry)
    CodeMuncher.generateGeneratorList()
  }

  def generate() = {
    for (entry <- GeneratorList.entries)
      entry.generate()
  }

  def duplicate() = {
    import Layer._

    all /* dummy */

//    for (entry <- GeneratorList.entries)
//      entry.duplicateFromTo(L2, L3)

//    ME_XXX.duplicateFromTo(IR, L2)
  }
}
