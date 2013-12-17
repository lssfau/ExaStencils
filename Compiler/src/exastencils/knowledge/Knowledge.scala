package exastencils.knowledge

object Knowledge {
  val numGhostLayers = 1;
  val maxLevel = 5;
  val numLevels = maxLevel + 1;
  val fragmentCommStrategy = 6;
  //val fragmentCommStrategy = 26;

  val numSolSlots = 1; // TODO: set according to smoother
  
  val numFragsPerBlock_x = 3;
  val numFragsPerBlock_y = 3;
  val numFragsPerBlock_z = 3;
}

object Globals {
  var printPath = s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Generated/";
}