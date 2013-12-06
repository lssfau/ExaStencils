package exastencils.primitives

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.primitives._

object GenCommCode extends (() => Unit) {
  def apply() : Unit = {
    var fragment = new FragmentClass;

    println("Setting up FragmentClass");

    // HACK
    StateManager.root_ = Root(List(fragment));

    var strategy = new Strategy("strategy");
    strategy += new Transformation({
      case frag : FragmentClass =>
        for (neigh <- frag.neighbors) {
          neigh.addDeclarations(frag);
        }

        frag.init;
        Some(frag);
    });

    strategy += new Transformation({
      case frag : FragmentClass =>
        println("Found a FragmentClass node");
        frag.fields += new Field("Solution", "solData", "double", "NUM_SOL_SLOTS", true);
        frag.fields += new Field("Residual", "resData", "double", "1", false);
        frag.fields += new Field("RHS", "rhsData", "double", "1", false);
        Some(frag);
    });

    strategy += new Transformation({
      case frag : FragmentClass =>
        frag.functions += new WaitForMPIReq;
        for (field <- frag.fields) {
          
          frag.functions += new ExchangeDataSplitter(field);
          for (level <- (0 to Knowledge.maxLevel)) {
            frag.functions += new ExchangeData_26(field, level);
          }
        }
        Some(frag);
    });

    strategy += new Transformation({
      case frag : FragmentClass =>
        frag.functions += new ConnectLocalElement();
        frag.functions += new ConnectRemoteElement();
        frag.functions += new SetupBuffers(frag.fields);
        Some(frag);
    });


    // 'actual' transformations
    strategy += new Transformation({
      case func : WaitForMPIReq =>
        println("Found a WaitForMPIReq node");
      	Some(func);
    });
    
    strategy += new Transformation({
      case func : HandleBoundaries =>
        println("Found a HandleBoundaries node");
      	Some(new Scope(func.neighbors.map(neigh => neigh.codeTreatBC).toArray));//FIXME: not working
    });
    
    
    // print
    strategy += new Transformation({
      case frag : FragmentClass =>
        frag.cpp;
        Some(frag);
    });

    
    strategy.apply;
    println("Done");
    
    //println(StateManager.root_.asInstanceOf[FragmentClass].fields);
  }
}