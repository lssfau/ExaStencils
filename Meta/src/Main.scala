import meta._

object Main {
  def main(args : Array[String]) : Unit = {
    collect()
    generate()
    duplicate()
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
    //ME_XXX.duplicateFor(IR, L2)
  }
}
