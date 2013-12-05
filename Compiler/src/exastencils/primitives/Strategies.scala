package exastencils.primitives

import exastencils.core._
import exastencils.datastructures._
import exastencils.primitives._

object GenCommCode extends (() => Unit) {
  def apply() : Unit = {
    var fragment = new FragmentClass;

    println("Setting up FragmentClass");

    // HACK
    StateManager.root_ = fragment;

    StateManager.apply(new Transformation({
      case frag : FragmentClass =>
        for (neigh <- frag.neighbors) {
          neigh.addDeclarations(frag);
        }

        frag.init;
        Some(frag);
    }));

    StateManager.apply(new Transformation({
      case frag : FragmentClass =>
        frag.fields += new Field("Solution", "solData", "double", "NUM_SOL_SLOTS", true);
        frag.fields += new Field("Residual", "resData", "double", "1", false);
        frag.fields += new Field("RHS", "rhsData", "double", "1", false);
        Some(frag);
    }));

    StateManager.apply(new Transformation({
      case frag : FragmentClass =>
        frag.functions += (new WaitForMPIReq).toString_cpp;
        for (field <- frag.fields) {
          val maxLevel = 9;
          frag.functions += (new ExchangeDataSplitter(field, maxLevel)).toString_cpp;
          for (level <- (0 to maxLevel)) {
            frag.functions += (new ExchangeData(field, level)).toString_cpp;
          }
        }
        Some(frag);
    }));

    StateManager.apply(new Transformation({
      case frag : FragmentClass =>
        frag.functions += (new ConnectLocalElement().toString_cpp);
        frag.functions += (new ConnectRemoteElement().toString_cpp);
        frag.functions += (new SetupBuffers(frag.fields).toString_cpp);
        Some(frag);
    }));

    StateManager.apply(new Transformation({
      case frag : FragmentClass =>
        frag.toString_cpp;
        Some(frag);
    }));

    println("Done");
  }
}