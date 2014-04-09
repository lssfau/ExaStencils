package exastencils.multiGrid

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._
import exastencils.mpi._
import exastencils.omp._

// TODO: think about offseting MultiIndices or stencil application nodes

case class PerformSmoothing_Jac(solutionField : Field, rhsField : Field, level : Int) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PerformSmoothingJacobi\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performSmoothing_$level", ListBuffer(VariableAccess("targetSlot", Some("unsigned int")), VariableAccess("sourceSlot", Some("unsigned int"))),
      ListBuffer(
        s"exchsolData_$level(sourceSlot);",
        new LoopOverFragments(
          new LoopOverDimensions(
            new IndexRange(
              new MultiIndex(solutionField.layout(0).idxDupLeftBegin, solutionField.layout(1).idxDupLeftBegin, solutionField.layout(2).idxDupLeftBegin),
              new MultiIndex(solutionField.layout(0).idxDupRightEnd, solutionField.layout(1).idxDupRightEnd, solutionField.layout(2).idxDupRightEnd)), ListBuffer[Statement](
              AssignmentStatement(
                // FIXME: introduce and apply stencil node
                DirectFieldAccess("curFragment.", solutionField, "targetSlot", DefaultLoopMultiIndex()),
                s"${1.0 - Knowledge.mg_smoother_omega} * " ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", DefaultLoopMultiIndex())
                  ~ s"+ ${Knowledge.mg_smoother_omega} / 6.0 * ("
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex(StringConstant("x") + 1, "y", "z"))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x" - 1, "y", "z"))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x", StringConstant("y") + 1, "z"))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x", "y" - 1, "z"))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x", "y", StringConstant("z") + 1))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x", "y", "z" - 1))
                  ~ s"-" ~ DirectFieldAccess("curFragment.", rhsField, "0", DefaultLoopMultiIndex())
                  ~ ")"))) with OMP_PotentiallyParallel) with OMP_PotentiallyParallel));
  }
}

case class PerformSmoothing_GS(solutionField : Field, rhsField : Field, level : Int) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PerformSmoothingJacobi\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performSmoothing_$level", ListBuffer(VariableAccess("targetSlot", Some("unsigned int")), VariableAccess("sourceSlot", Some("unsigned int"))),
      ListBuffer(
        s"exchsolData_$level(sourceSlot);",
        new LoopOverFragments(
          new LoopOverDimensions(
            new IndexRange(
              new MultiIndex(solutionField.layout(0).idxDupLeftBegin, solutionField.layout(1).idxDupLeftBegin, solutionField.layout(2).idxDupLeftBegin),
              new MultiIndex(solutionField.layout(0).idxDupRightEnd, solutionField.layout(1).idxDupRightEnd, solutionField.layout(2).idxDupRightEnd)),
            ListBuffer[Statement](
              AssignmentStatement(
                // FIXME: introduce and apply stencil node
                DirectFieldAccess("curFragment.", solutionField, "targetSlot", DefaultLoopMultiIndex()),
                s"${1.0 - Knowledge.mg_smoother_omega} * " ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", DefaultLoopMultiIndex())
                  ~ s"+ ${Knowledge.mg_smoother_omega} / 6.0 * ("
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex(StringConstant("x") + 1, "y", "z"))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x" - 1, "y", "z"))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x", StringConstant("y") + 1, "z"))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x", "y" - 1, "z"))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x", "y", StringConstant("z") + 1))
                  ~ s"+" ~ DirectFieldAccess("curFragment.", solutionField, "sourceSlot", new MultiIndex("x", "y", "z" - 1))
                  ~ s"-" ~ DirectFieldAccess("curFragment.", rhsField, "0", DefaultLoopMultiIndex())
                  ~ ")")))) with OMP_PotentiallyParallel));
  }
}

case class PerformSmoothing(solutionField : Field, rhsField : Field, level : Int) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PerformSmoothing\n";

  override def expand(collector : StackCollector) : AbstractFunctionStatement = {
    Knowledge.mg_smoother match {
      case SmootherType.Jac => new PerformSmoothing_Jac(solutionField, rhsField, level)
      case SmootherType.GS  => new PerformSmoothing_GS(solutionField, rhsField, level)
    }
  }

  /* FIXME: re-integrate this code in separate nodes
    FunctionStatement(new UnitDatatype(), s"smootherIteration_Node", ListBuffer(Variable("unsigned int", "targetSlot"), Variable("unsigned int", "sourceSlot"), Variable("unsigned int", "level")),
      ListBuffer(
        """
	double h = 1.0;// / (1u << level);
	double hSq = h * h;
	double hSqInv = 1.0 / hSq;

	exchsolData(level, sourceSlot);

#ifdef SMOOTHER_GSOD
	for (unsigned int smootherIteration = 0; smootherIteration < NUM_GSOD_ITERATIONS; ++smootherIteration)
	{
#endif

		// solver iteration (elements)
#pragma omp parallel for schedule(static, 1)
		for (int f = 0; f < fragments.size(); ++f)	// unsigned wont work... really, OMP?
		{
			// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
			const double*	solSrcData	= fragments[f]->solData[sourceSlot][level]->getDataPtr();
			double*			solDestData	= fragments[f]->solData[targetSlot][level]->getDataPtr();
			const double*	rhsData		= fragments[f]->rhsData[0][level]->getDataPtr();
			Vec3u				dimWOPad	= fragments[f]->solData[0][level]->numDataPointsPerDim;
			Vec3u				dimWPad		= fragments[f]->solData[0][level]->numDataPointsPerDimWPad;
			Vec3u				first		= fragments[f]->solData[0][level]->firstDataPoint;
			Vec3u				last		= fragments[f]->solData[0][level]->lastDataPoint;

#ifdef SMOOTHER_GSACBE
			Vec3u				numBlocks	= (dimWOPad - Vec3u(2 * """ + s"${Knowledge.data_numGhostLayers}" + """) + Vec3u(GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP - 1)) / Vec3u(GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP) + Vec3u(1);

			for (unsigned int bz = 0; bz < numBlocks.z; ++bz)
				for (unsigned int by = 0; by < numBlocks.y; ++by)
					for (unsigned int bx = 0; bx < numBlocks.x; ++bx)
					{
						unsigned int zMin = """ + s"${Knowledge.data_numGhostLayers}" + """ + bz * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
						unsigned int zMax = std::min(zMin + GSBE_WINDOW_SIZE, dimWOPad.z - """ + s"${Knowledge.data_numGhostLayers}" + """);
						unsigned int yMin = """ + s"${Knowledge.data_numGhostLayers}" + """ + by * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
						unsigned int yMax = std::min(yMin + GSBE_WINDOW_SIZE, dimWOPad.y - """ + s"${Knowledge.data_numGhostLayers}" + """);
						unsigned int xMin = """ + s"${Knowledge.data_numGhostLayers}" + """ + bx * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
						unsigned int xMax = std::min(xMin + GSBE_WINDOW_SIZE, dimWOPad.x - """ + s"${Knowledge.data_numGhostLayers}" + """);

						for (unsigned int i = 0; i < NUM_GSBE_ITERATIONS; ++i)
						{
							for (unsigned int z = zMin + first.z; z < zMax + first.z; ++z)
								for (unsigned int y = yMin + first.y; y < yMax + first.y; ++y)
									for (unsigned int x = xMin + first.x; x < xMax + first.x; ++x)
									{
										unsigned int posAbs = z * dimWPad.xy().componentProd() + y * dimWPad.x + x;
										solDestData[posAbs] = 
											(1.0 - """ + s"${Knowledge.smootherOmega}" + """) * solSrcData[posAbs]
										+ """ + s"${Knowledge.smootherOmega}" + """ * 1.0 / 6.0 * (
											solSrcData[posAbs - 1] + solSrcData[posAbs + 1]
										+ solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x]
										+ solSrcData[posAbs - dimWPad.x * dimWPad.y] + solSrcData[posAbs + dimWPad.x * dimWPad.y]
										- rhsData[posAbs]);
									}
						}
					}
#elif defined(SMOOTHER_GSRS)
			for (unsigned int randCnt = 0; randCnt < 2 * dimWOPad.componentProd(); ++randCnt)
			{
				unsigned int x = (std::rand() % (dimWOPad.x - 2)) + 1 + first.x;
				unsigned int y = (std::rand() % (dimWOPad.y - 2)) + 1 + first.y;
				unsigned int posAbs = y * dimWPad.x + x;

				solDestData[posAbs] = 
					(1.0 - """ + s"${Knowledge.smootherOmega}" + """) * solSrcData[posAbs]
				+ """ + s"${Knowledge.smootherOmega}" + """ * 0.25 * (solSrcData[posAbs - 1] + solSrcData[posAbs + 1] + solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x] - rhsData[posAbs]);
			}
#elif defined (SMOOTHER_GSRB) || defined (SMOOTHER_GSRBAC)
			for (unsigned int z = first.z + """ + s"${Knowledge.data_numGhostLayers}" + """; z <= last.z - """ + s"${Knowledge.data_numGhostLayers}" + """; ++z)
			{
				for (unsigned int y = first.y + """ + s"${Knowledge.data_numGhostLayers}" + """; y <= last.y - """ + s"${Knowledge.data_numGhostLayers}" + """; ++y)
				{
					unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + """ + s"${Knowledge.data_numGhostLayers}" + """ + ((y - first.y - """ + s"${Knowledge.data_numGhostLayers}" + """) % 2);
					for (unsigned int x = first.x + """ + s"${Knowledge.data_numGhostLayers}" + """ + ((y - first.y - """ + s"${Knowledge.data_numGhostLayers}" + """) % 2); x <= last.x - """ + s"${Knowledge.data_numGhostLayers}" + """; x += 2, posAbs += 2)
					{
						solDestData[posAbs] = 
							(1.0 - """ + s"${Knowledge.smootherOmega}" + """) * solSrcData[posAbs]
						+ """ + s"${Knowledge.smootherOmega}" + """ * 1.0 / 6.0 * (
							solSrcData[posAbs - 1] + solSrcData[posAbs + 1]
						+ solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x]
						+ solSrcData[posAbs - dimWPad.x * dimWPad.y] + solSrcData[posAbs + dimWPad.x * dimWPad.y]
						- rhsData[posAbs]);
					}
				}
			}

#	ifdef USE_ADDITIONAL_SMOOTHER_COMM
			exchsolData(level, targetSlot);
#	endif

			for (unsigned int z = first.z + """ + s"${Knowledge.data_numGhostLayers}" + """; z <= last.z - """ + s"${Knowledge.data_numGhostLayers}" + """; ++z)
			{
				for (unsigned int y = first.y + """ + s"${Knowledge.data_numGhostLayers}" + """; y <= last.y - """ + s"${Knowledge.data_numGhostLayers}" + """; ++y)
				{
					unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + """ + s"${Knowledge.data_numGhostLayers}" + """ + ((y - first.y - """ + s"${Knowledge.data_numGhostLayers}" + """ + 1) % 2);
					for (unsigned int x = first.x + """ + s"${Knowledge.data_numGhostLayers}" + """ + ((y - first.y - """ + s"${Knowledge.data_numGhostLayers}" + """ + 1) % 2); x <= last.x - """ + s"${Knowledge.data_numGhostLayers}" + """; x += 2, posAbs += 2)
					{
						solDestData[posAbs] = 
							(1.0 - """ + s"${Knowledge.smootherOmega}" + """) * solSrcData[posAbs]
						+ """ + s"${Knowledge.smootherOmega}" + """ * 1.0 / 6.0 * (
							solSrcData[posAbs - 1] + solSrcData[posAbs + 1]
						+ solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x]
						+ solSrcData[posAbs - dimWPad.x * dimWPad.y] + solSrcData[posAbs + dimWPad.x * dimWPad.y]
						- rhsData[posAbs]);
					}
				}
			}
#endif
		}

#ifdef SMOOTHER_GSOD
	}
#endif
"""));
  }
  */
}

case class UpdateResidual(residualField : Field, solutionField : Field, rhsField : Field, level : Int) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = UpdateResidual\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"updateResidual_$level", ListBuffer(VariableAccess("slot", Some("unsigned int"))),
      ListBuffer(
        s"exchsolData_$level(slot);",
        new LoopOverFragments(
          new LoopOverDimensions(
            new IndexRange(
              new MultiIndex(residualField.layout(0).idxDupLeftBegin, residualField.layout(1).idxDupLeftBegin, residualField.layout(2).idxDupLeftBegin),
              new MultiIndex(residualField.layout(0).idxDupRightEnd, residualField.layout(1).idxDupRightEnd, residualField.layout(2).idxDupRightEnd)),
            ListBuffer[Statement](
              AssignmentStatement(
                // FIXME: introduce and apply stencil node
                DirectFieldAccess("curFragment.", residualField, "0", DefaultLoopMultiIndex()),
                DirectFieldAccess("curFragment.", rhsField, "0", DefaultLoopMultiIndex())
                  ~ s"-" ~ DirectFieldAccess("curFragment.", solutionField, "slot", new MultiIndex(StringConstant("x") + 1, "y", "z"))
                  ~ s"-" ~ DirectFieldAccess("curFragment.", solutionField, "slot", new MultiIndex("x" - 1, "y", "z"))
                  ~ s"-" ~ DirectFieldAccess("curFragment.", solutionField, "slot", new MultiIndex("x", StringConstant("y") + 1, "z"))
                  ~ s"-" ~ DirectFieldAccess("curFragment.", solutionField, "slot", new MultiIndex("x", "y" - 1, "z"))
                  ~ s"-" ~ DirectFieldAccess("curFragment.", solutionField, "slot", new MultiIndex("x", "y", StringConstant("z") + 1))
                  ~ s"-" ~ DirectFieldAccess("curFragment.", solutionField, "slot", new MultiIndex("x", "y", "z" - 1))
                  ~ s"+ 6.0 * " ~ DirectFieldAccess("curFragment.", solutionField, "slot", DefaultLoopMultiIndex())))) with OMP_PotentiallyParallel) with OMP_PotentiallyParallel));
  }
}

case class PerformRestriction() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PerformRestriction\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performRestriction_NodeFW", ListBuffer(VariableAccess("levelSrc", Some("unsigned int")), VariableAccess("levelDest", Some("unsigned int"))),
      ListBuffer(
        s"exchresData(levelSrc, 0);",
        new LoopOverFragments(
          """
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		const double*	srcData		= fragments[f]->resData[0][levelSrc]->getDataPtr();
		double*			destData	= fragments[f]->rhsData[0][levelDest]->getDataPtr();
		Vec3u				dimWPadSrc	= fragments[f]->solData[0][levelSrc]->numDataPointsPerDimWPad;
		Vec3u				firstSrc	= fragments[f]->solData[0][levelSrc]->firstDataPoint;
		Vec3u				lastSrc		= fragments[f]->solData[0][levelSrc]->lastDataPoint;
		Vec3u				dimWPadDest	= fragments[f]->solData[0][levelDest]->numDataPointsPerDimWPad;
		Vec3u				firstDest	= fragments[f]->solData[0][levelDest]->firstDataPoint;
		Vec3u				lastDest	= fragments[f]->solData[0][levelDest]->lastDataPoint;

		unsigned int		dxS = 1;
		unsigned int		dyS	= dimWPadSrc.x;
		unsigned int		dzS	= dimWPadSrc.x * dimWPadSrc.y;

		for (unsigned int z = firstDest.z + """ + s"${Knowledge.data_numGhostLayers}" + """; z <= lastDest.z - """ + s"${Knowledge.data_numGhostLayers}" + """; ++z)
		{
			for (unsigned int y = firstDest.y + """ + s"${Knowledge.data_numGhostLayers}" + """; y <= lastDest.y - """ + s"${Knowledge.data_numGhostLayers}" + """; ++y)
			{
				unsigned int posAbs = (2 * (z - firstDest.z - """ + s"${Knowledge.data_numGhostLayers}" + """) + firstSrc.z + """ + s"${Knowledge.data_numGhostLayers}" + """) * dzS
					+ (2 * (y - firstDest.y - """ + s"${Knowledge.data_numGhostLayers}" + """) + firstSrc.y + """ + s"${Knowledge.data_numGhostLayers}" + """) * dyS
					+ firstSrc.x + """ + s"${Knowledge.data_numGhostLayers}" + """;
				for (unsigned int x = firstDest.x + """ + s"${Knowledge.data_numGhostLayers}" + """; x <= lastDest.x - """ + s"${Knowledge.data_numGhostLayers}" + """; ++x, posAbs += 2)
				{
					destData[z * dimWPadDest.y * dimWPadDest.x + y * dimWPadDest.x + x] =
						+ 1.0 * (
						srcData[posAbs])						// 1
						+ 0.5 * (
						srcData[posAbs - dxS]
					+ srcData[posAbs + dxS]
					+ srcData[posAbs - dyS]
					+ srcData[posAbs + dyS]
					+ srcData[posAbs - dzS]
					+ srcData[posAbs + dzS])				// 3
						+ 0.25 * (
						srcData[posAbs - dyS - dxS]
					+ srcData[posAbs - dyS + dxS]
					+ srcData[posAbs + dyS - dxS]
					+ srcData[posAbs + dyS + dxS]
					+ srcData[posAbs - dzS - dxS]
					+ srcData[posAbs - dzS + dxS]
					+ srcData[posAbs + dzS - dxS]
					+ srcData[posAbs + dzS + dxS]
					+ srcData[posAbs - dzS - dyS]
					+ srcData[posAbs - dzS + dyS]
					+ srcData[posAbs + dzS - dyS]
					+ srcData[posAbs + dzS + dyS])			// 3
						+ 0.125 * (
						srcData[posAbs - dzS - dyS - dxS]
					+ srcData[posAbs - dzS - dyS + dxS]
					+ srcData[posAbs - dzS + dyS - dxS]
					+ srcData[posAbs - dzS + dyS + dxS]
					+ srcData[posAbs + dzS - dyS - dxS]
					+ srcData[posAbs + dzS - dyS + dxS]
					+ srcData[posAbs + dzS + dyS - dxS]
					+ srcData[posAbs + dzS + dyS + dxS]);	// 1
					destData[z * dimWPadDest.x * dimWPadDest.y + y * dimWPadDest.x + x] *= 0.5;
				}
			}
		}
""") with OMP_PotentiallyParallel));
  }
}

case class PerformProlongation() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PerformProlongation\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performProlongation_NodeTLI", ListBuffer(VariableAccess("slotSrc", Some("unsigned int")), VariableAccess("levelSrc", Some("unsigned int")), VariableAccess("slotDest", Some("unsigned int")), VariableAccess("levelDest", Some("unsigned int"))),
      ListBuffer(
        s"exchsolData(levelSrc, slotSrc);",
        new LoopOverFragments(
          """
		// WARNING: assumes an odd number of data points per dimension (e.g. 2^l + 1) or a number smaller or equal to 2
		const double*	srcData		= fragments[f]->solData[slotSrc][levelSrc]->getDataPtr();
		double*			destData	= fragments[f]->solData[slotDest][levelDest]->getDataPtr();
		Vec3u				dimWPadSrc	= fragments[f]->solData[0][levelSrc]->numDataPointsPerDimWPad;
		Vec3u				firstSrc	= fragments[f]->solData[0][levelSrc]->firstDataPoint;
		Vec3u				lastSrc		= fragments[f]->solData[0][levelSrc]->lastDataPoint;
		Vec3u				dimWPadDest	= fragments[f]->solData[0][levelDest]->numDataPointsPerDimWPad;
		Vec3u				firstDest	= fragments[f]->solData[0][levelDest]->firstDataPoint;
		Vec3u				lastDest	= fragments[f]->solData[0][levelDest]->lastDataPoint;

		unsigned int		dxS = 1;
		unsigned int		dyS	= dimWPadSrc.x;
		unsigned int		dzS	= dimWPadSrc.x * dimWPadSrc.y;
		unsigned int		dxD = 1;
		unsigned int		dyD	= dimWPadDest.x;
		unsigned int		dzD	= dimWPadDest.x * dimWPadDest.y;

		for (unsigned int z = firstDest.z + """ + s"${Knowledge.data_numGhostLayers}" + """; z <= lastDest.z - """ + s"${Knowledge.data_numGhostLayers}" + """; z += 2)
		{
			for (unsigned int y = firstDest.y + """ + s"${Knowledge.data_numGhostLayers}" + """; y <= lastDest.y - """ + s"${Knowledge.data_numGhostLayers}" + """; y += 2)
			{
				unsigned int destPos = z * dzD + y * dyD + firstDest.x + """ + s"${Knowledge.data_numGhostLayers}" + """;
				unsigned int srcPos = (((z - firstDest.z - """ + s"${Knowledge.data_numGhostLayers}" + """) >> 1) + firstSrc.z + """ + s"${Knowledge.data_numGhostLayers}" + """) * dzS
					+ (((y - firstDest.y - """ + s"${Knowledge.data_numGhostLayers}" + """) >> 1) + firstSrc.y + """ + s"${Knowledge.data_numGhostLayers}" + """) * dyS
					+ firstSrc.x + """ + s"${Knowledge.data_numGhostLayers}" + """;
				for (unsigned int x = firstDest.x + """ + s"${Knowledge.data_numGhostLayers}" + """; x <= lastDest.x - """ + s"${Knowledge.data_numGhostLayers}" + """; x += 2, destPos += 2, ++srcPos)
				{
					destData[destPos] += 1.0 * (
						srcData[srcPos]);

					if (x < lastDest.x - """ + s"${Knowledge.data_numGhostLayers}" + """)
						destData[destPos + dxD] += 0.5 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]);

					if (y < lastDest.y - """ + s"${Knowledge.data_numGhostLayers}" + """)
						destData[destPos + dyD] += 0.5 * (
						srcData[srcPos]
					+ srcData[srcPos + dyS]);

					if (z < lastDest.z - """ + s"${Knowledge.data_numGhostLayers}" + """)
						destData[destPos + dzD] += 0.5 * (
						srcData[srcPos]
					+ srcData[srcPos + dzS]);

					if (x < lastDest.x - """ + s"${Knowledge.data_numGhostLayers}" + """ && y < lastDest.y - """ + s"${Knowledge.data_numGhostLayers}" + """)
						destData[destPos + dyD + dxD] += 0.25 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]
					+ srcData[srcPos + dyS]
					+ srcData[srcPos + dyS + dxS]);

					if (x < lastDest.x - """ + s"${Knowledge.data_numGhostLayers}" + """ && z < lastDest.z - """ + s"${Knowledge.data_numGhostLayers}" + """)
						destData[destPos + dzD + dxD] += 0.25 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]
					+ srcData[srcPos + dzS]
					+ srcData[srcPos + dzS + dxS]);

					if (y < lastDest.y - """ + s"${Knowledge.data_numGhostLayers}" + """ && z < lastDest.z - """ + s"${Knowledge.data_numGhostLayers}" + """)
						destData[destPos + dzD + dyD] += 0.25 * (
						srcData[srcPos]
					+ srcData[srcPos + dyS]
					+ srcData[srcPos + dzS]
					+ srcData[srcPos + dzS + dyS]);

					if (x < lastDest.x - """ + s"${Knowledge.data_numGhostLayers}" + """ && y < lastDest.y - """ + s"${Knowledge.data_numGhostLayers}" + """ && z < lastDest.z - """ + s"${Knowledge.data_numGhostLayers}" + """)
						destData[destPos + dzD + dyD + dxD] += 0.125 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]
					+ srcData[srcPos + dyS]
					+ srcData[srcPos + dyS + dxS]
					+ srcData[srcPos + dzS]
					+ srcData[srcPos + dzS + dxS]
					+ srcData[srcPos + dzS + dyS]
					+ srcData[srcPos + dzS + dyS + dxS]);
				}
			}
		}
""") with OMP_PotentiallyParallel));
  }
}

case class PerformCGS(level : Int) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PerformVCycle\n";

  override def expand(collector : StackCollector) : ForLoopStatement = {
    Knowledge.mg_cgs match {
      case CoarseGridSolverType.IP_Smoother =>
        ForLoopStatement(s"unsigned int s = 0", s"s < ${Knowledge.mg_cgs_numSteps}", s"++s", ListBuffer(
          s"++solSlots[0];",
          s"performSmoothing_$level((solSlots[$level] + 0) % ${Knowledge.data_numSolSlots}, (solSlots[$level] - 1) % ${Knowledge.data_numSolSlots});"))
    }
  }
}

case class PerformVCycle(level : Int) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PerformVCycle\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performVCycle_$level", ListBuffer(VariableAccess("solSlots", Some("unsigned int*"))),
      (if (0 == level) {
        ListBuffer[Statement](new PerformCGS(level))
      } else {
        ListBuffer[Statement](
          ForLoopStatement(s"unsigned int s = 0", s"s < ${Knowledge.mg_smoother_numPre}", s"++s", ListBuffer(
            s"++solSlots[$level];",
            s"performSmoothing_$level((solSlots[$level] + 0) % ${Knowledge.data_numSolSlots}, (solSlots[$level] - 1) % ${Knowledge.data_numSolSlots});")),

          s"updateResidual_$level(solSlots[$level] % ${Knowledge.data_numSolSlots});",

          s"performRestriction_NodeFW($level, ${level - 1});",

          s"setSolZero_${level - 1}(solSlots[${level - 1}] % ${Knowledge.data_numSolSlots});",

          s"performVCycle_${level - 1}(solSlots);",

          s"performProlongation_NodeTLI(solSlots[${level - 1}] % ${Knowledge.data_numSolSlots}, ${level - 1}, solSlots[$level] % ${Knowledge.data_numSolSlots}, $level);",

          ForLoopStatement(s"unsigned int s = 0", s"s < ${Knowledge.mg_smoother_numPost}", s"++s", ListBuffer(
            s"++solSlots[$level];",
            s"performSmoothing_$level((solSlots[$level] + 0) % ${Knowledge.data_numSolSlots}, (solSlots[$level] - 1) % ${Knowledge.data_numSolSlots});")))
      })
        ++
        (if (Knowledge.maxLevel == level) { ListBuffer[Statement](s"updateResidual_$level(solSlots[$level] % ${Knowledge.data_numSolSlots});") }
        else { ListBuffer[Statement]() }));
  }
}

case class GetGlobalResidual(field : Field) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = GetGlobalResidual\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement("double", s"getGlobalResidual_${field.level}", ListBuffer(),
      ListBuffer[Statement](
        s"double res = 0.0;",
        s"double resTotal = 0.0;",
        new LoopOverFragments(
          new LoopOverDimensions(
            new IndexRange(
              new MultiIndex(field.layout(0).idxDupLeftBegin, field.layout(1).idxDupLeftBegin, field.layout(2).idxDupLeftBegin),
              new MultiIndex(field.layout(0).idxDupRightEnd, field.layout(1).idxDupRightEnd, field.layout(2).idxDupRightEnd)),
            ListBuffer[Statement](
              // FIXME: this currently counts duplicated values multiple times
              s"double tmpRes =" ~ new DirectFieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()) ~ ";",
              s"res += tmpRes * tmpRes;"), "reduction(+:res)") with OMP_PotentiallyParallel,
          true, "reduction(+:res)") with OMP_PotentiallyParallel,
        new MPI_Allreduce("&res", "&resTotal", 1, "MPI_SUM"),
        s"return sqrt(resTotal);"));
  }
}

case class SetSolZero(field : Field, level : Int) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetSolZero\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    new FunctionStatement(new UnitDatatype(), s"setSolZero_$level", ListBuffer(VariableAccess("slot", Some("unsigned int"))),
      new LoopOverFragments(
        new LoopOverDimensions(
          new IndexRange(
            new MultiIndex(field.layout(0).idxDupLeftBegin, field.layout(1).idxDupLeftBegin, field.layout(2).idxDupLeftBegin),
            new MultiIndex(field.layout(0).idxDupRightEnd, field.layout(1).idxDupRightEnd, field.layout(2).idxDupRightEnd)),
          new AssignmentStatement(
            new DirectFieldAccess("curFragment.", field, "slot", DefaultLoopMultiIndex()),
            0.0)) with OMP_PotentiallyParallel) with OMP_PotentiallyParallel);
  }
}
