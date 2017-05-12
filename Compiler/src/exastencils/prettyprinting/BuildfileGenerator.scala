package exastencils.prettyprinting

import scala.collection.mutable.ListBuffer

import exastencils.logger.Logger

object BuildfileGenerator {
  def parseGenerators(generators : ListBuffer[String]) : ListBuffer[BuildfileGenerator] = {
    val buildfileGeneratorMap = Map(
      "CMakeGenerator" -> CMakeGenerator,
      "MakefileGenerator" -> MakefileGenerator,
      "ProjectfileGenerator" -> ProjectfileGenerator)

    // check for invalid generators
    for (generator <- generators)
      if (!buildfileGeneratorMap.contains(generator)) {
        Logger.warn("Ignoring invalid buildfile generator: " + generator)
        generators -= generator
      }

    // default to MakefileGenerator in case of empty lists
    // TODO: this should not be necessary - users should be allowed to simply generate source code files
    if (generators.isEmpty) {
      Logger.warn("No buildfile generator specified; defaulting to MakefileGenerator")
      generators += "MakefileGenerator"
    }

    generators.distinct.map(buildfileGeneratorMap(_))
  }
}

trait BuildfileGenerator {
  def write() : Unit
}
