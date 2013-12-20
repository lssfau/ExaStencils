package exastencils.multiGrid

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._
import exastencils.mpi._

case class PerformSmoothing_Jac(solutionField : Field, rhsField : Field, level : Integer) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformSmoothingJacobi\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performSmoothing_$level", ListBuffer(Variable(s"Fragment3DCube*", s"fragments[${Knowledge.numFragsPerBlock}]"), Variable("unsigned int", "targetSlot"), Variable("unsigned int", "sourceSlot")),
      ListBuffer(
        s"exchsolData(fragments, $level, sourceSlot);",
        new LoopOverFragments(
          new LoopOverDimensions(
            fieldToIndexInner(Array(0, 0, 0), level), ListBuffer[Statement](
              AssignmentStatement(
                // FIXME: introduce and apply stencil node
                FieldAccess(solutionField, level, "targetSlot", Mapping.access(level)),
                s"${1.0 - Knowledge.smootherOmega} * " ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level))
                  ~ s"+ ${Knowledge.smootherOmega} / 6.0 * ("
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "z", "y", "(x + 1)"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "z", "y", "(x - 1)"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "z", "(y + 1)", "x"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "z", "(y - 1)", "x"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "(z - 1)", "y", "x"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "(z + 1)", "y", "x"))
                  ~ s"-" ~ FieldAccess(rhsField, level, "0", Mapping.access(level))
                  ~ ")"))))));
  }
}

case class PerformSmoothing_GS(solutionField : Field, rhsField : Field, level : Integer) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformSmoothingJacobi\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performSmoothing_$level", ListBuffer(Variable(s"Fragment3DCube*", s"fragments[${Knowledge.numFragsPerBlock}]"), Variable("unsigned int", "targetSlot"), Variable("unsigned int", "sourceSlot")),
      ListBuffer(
        s"exchsolData(fragments, $level, sourceSlot);",
        new LoopOverFragments(
          new LoopOverDimensions(
            fieldToIndexInner(Array(0, 0, 0), level), ListBuffer[Statement](
              AssignmentStatement(
                // FIXME: introduce and apply stencil node
                FieldAccess(solutionField, level, "targetSlot", Mapping.access(level)),
                s"${1.0 - Knowledge.smootherOmega} * " ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level))
                  ~ s"+ ${Knowledge.smootherOmega} / 6.0 * ("
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "z", "y", "(x + 1)"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "z", "y", "(x - 1)"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "z", "(y + 1)", "x"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "z", "(y - 1)", "x"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "(z - 1)", "y", "x"))
                  ~ s"+" ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level, "(z + 1)", "y", "x"))
                  ~ s"-" ~ FieldAccess(rhsField, level, "0", Mapping.access(level))
                  ~ ")"))))));
  }
}

case class PerformSmoothing(solutionField : Field, rhsField : Field, level : Integer) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformSmoothing\n";

  override def expand : AbstractFunctionStatement = {
    Knowledge.smoother match {
      case SmootherType.Jac => new PerformSmoothing_Jac(solutionField, rhsField, level)
      case SmootherType.GS  => new PerformSmoothing_GS(solutionField, rhsField, level)
    }
  }

  /* FIXME: re-integrate this code in separate nodes
    FunctionStatement(new UnitDatatype(), s"smootherIteration_Node", ListBuffer(Variable(s"Fragment3DCube*", s"fragments[${Knowledge.numFragsPerBlock}]"), Variable("unsigned int", "targetSlot"), Variable("unsigned int", "sourceSlot"), Variable("unsigned int", "level")),
      ListBuffer(
        """
	double h = 1.0;// / (1u << level);
	double hSq = h * h;
	double hSqInv = 1.0 / hSq;

	exchsolData(fragments, level, sourceSlot);

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
			Vec3u				numBlocks	= (dimWOPad - Vec3u(2 * """ + s"${Knowledge.numGhostLayers}" + """) + Vec3u(GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP - 1)) / Vec3u(GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP) + Vec3u(1);

			for (unsigned int bz = 0; bz < numBlocks.z; ++bz)
				for (unsigned int by = 0; by < numBlocks.y; ++by)
					for (unsigned int bx = 0; bx < numBlocks.x; ++bx)
					{
						unsigned int zMin = """ + s"${Knowledge.numGhostLayers}" + """ + bz * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
						unsigned int zMax = std::min(zMin + GSBE_WINDOW_SIZE, dimWOPad.z - """ + s"${Knowledge.numGhostLayers}" + """);
						unsigned int yMin = """ + s"${Knowledge.numGhostLayers}" + """ + by * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
						unsigned int yMax = std::min(yMin + GSBE_WINDOW_SIZE, dimWOPad.y - """ + s"${Knowledge.numGhostLayers}" + """);
						unsigned int xMin = """ + s"${Knowledge.numGhostLayers}" + """ + bx * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
						unsigned int xMax = std::min(xMin + GSBE_WINDOW_SIZE, dimWOPad.x - """ + s"${Knowledge.numGhostLayers}" + """);

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
			for (unsigned int z = first.z + """ + s"${Knowledge.numGhostLayers}" + """; z <= last.z - """ + s"${Knowledge.numGhostLayers}" + """; ++z)
			{
				for (unsigned int y = first.y + """ + s"${Knowledge.numGhostLayers}" + """; y <= last.y - """ + s"${Knowledge.numGhostLayers}" + """; ++y)
				{
					unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + """ + s"${Knowledge.numGhostLayers}" + """ + ((y - first.y - """ + s"${Knowledge.numGhostLayers}" + """) % 2);
					for (unsigned int x = first.x + """ + s"${Knowledge.numGhostLayers}" + """ + ((y - first.y - """ + s"${Knowledge.numGhostLayers}" + """) % 2); x <= last.x - """ + s"${Knowledge.numGhostLayers}" + """; x += 2, posAbs += 2)
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
			exchsolData(fragments, level, targetSlot);
#	endif

			for (unsigned int z = first.z + """ + s"${Knowledge.numGhostLayers}" + """; z <= last.z - """ + s"${Knowledge.numGhostLayers}" + """; ++z)
			{
				for (unsigned int y = first.y + """ + s"${Knowledge.numGhostLayers}" + """; y <= last.y - """ + s"${Knowledge.numGhostLayers}" + """; ++y)
				{
					unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + """ + s"${Knowledge.numGhostLayers}" + """ + ((y - first.y - """ + s"${Knowledge.numGhostLayers}" + """ + 1) % 2);
					for (unsigned int x = first.x + """ + s"${Knowledge.numGhostLayers}" + """ + ((y - first.y - """ + s"${Knowledge.numGhostLayers}" + """ + 1) % 2); x <= last.x - """ + s"${Knowledge.numGhostLayers}" + """; x += 2, posAbs += 2)
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

case class UpdateResidual(residualField : Field, solutionField : Field, rhsField : Field, level : Integer) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = UpdateResidual\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"updateResidual_$level", ListBuffer(Variable(s"Fragment3DCube*", s"fragments[${Knowledge.numFragsPerBlock}]"), Variable("unsigned int", "slot")),
      ListBuffer(
        s"exchsolData(fragments, $level, slot);",
        new LoopOverFragments(
          new LoopOverDimensions(
            fieldToIndexInner(Array(0, 0, 0), level), ListBuffer[Statement](
              AssignmentStatement(
                // FIXME: introduce and apply stencil node
                FieldAccess(residualField, level, "0", Mapping.access(level)),
                FieldAccess(rhsField, level, "0", Mapping.access(level))
                  ~ s"-" ~ FieldAccess(solutionField, level, "slot", Mapping.access(level, "z", "y", "(x + 1)"))
                  ~ s"-" ~ FieldAccess(solutionField, level, "slot", Mapping.access(level, "z", "y", "(x - 1)"))
                  ~ s"-" ~ FieldAccess(solutionField, level, "slot", Mapping.access(level, "z", "(y + 1)", "x"))
                  ~ s"-" ~ FieldAccess(solutionField, level, "slot", Mapping.access(level, "z", "(y - 1)", "x"))
                  ~ s"-" ~ FieldAccess(solutionField, level, "slot", Mapping.access(level, "(z - 1)", "y", "x"))
                  ~ s"-" ~ FieldAccess(solutionField, level, "slot", Mapping.access(level, "(z + 1)", "y", "x"))
                  ~ s"+ 6.0 * " ~ FieldAccess(solutionField, level, "slot", Mapping.access(level))))))));
  }
}

case class PerformRestriction() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformRestriction\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performRestriction_NodeFW", ListBuffer(Variable(s"Fragment3DCube*", s"fragments[${Knowledge.numFragsPerBlock}]"), Variable("unsigned int", "levelSrc"), Variable("unsigned int", "levelDest")),
      ListBuffer(
        s"exchresData(fragments, levelSrc, 0);",
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

		for (unsigned int z = firstDest.z + """ + s"${Knowledge.numGhostLayers}" + """; z <= lastDest.z - """ + s"${Knowledge.numGhostLayers}" + """; ++z)
		{
			for (unsigned int y = firstDest.y + """ + s"${Knowledge.numGhostLayers}" + """; y <= lastDest.y - """ + s"${Knowledge.numGhostLayers}" + """; ++y)
			{
				unsigned int posAbs = (2 * (z - firstDest.z - """ + s"${Knowledge.numGhostLayers}" + """) + firstSrc.z + """ + s"${Knowledge.numGhostLayers}" + """) * dzS
					+ (2 * (y - firstDest.y - """ + s"${Knowledge.numGhostLayers}" + """) + firstSrc.y + """ + s"${Knowledge.numGhostLayers}" + """) * dyS
					+ firstSrc.x + """ + s"${Knowledge.numGhostLayers}" + """;
				for (unsigned int x = firstDest.x + """ + s"${Knowledge.numGhostLayers}" + """; x <= lastDest.x - """ + s"${Knowledge.numGhostLayers}" + """; ++x, posAbs += 2)
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
""")));
  }
}

case class PerformProlongation() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformProlongation\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performProlongation_NodeTLI", ListBuffer(Variable(s"Fragment3DCube*", s"fragments[${Knowledge.numFragsPerBlock}]"), Variable("unsigned int", "slotSrc"), Variable("unsigned int", "levelSrc"), Variable("unsigned int", "slotDest"), Variable("unsigned int", "levelDest")),
      ListBuffer(
        s"exchsolData(fragments, levelSrc, slotSrc);",
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

		for (unsigned int z = firstDest.z + """ + s"${Knowledge.numGhostLayers}" + """; z <= lastDest.z - """ + s"${Knowledge.numGhostLayers}" + """; z += 2)
		{
			for (unsigned int y = firstDest.y + """ + s"${Knowledge.numGhostLayers}" + """; y <= lastDest.y - """ + s"${Knowledge.numGhostLayers}" + """; y += 2)
			{
				unsigned int destPos = z * dzD + y * dyD + firstDest.x + """ + s"${Knowledge.numGhostLayers}" + """;
				unsigned int srcPos = (((z - firstDest.z - """ + s"${Knowledge.numGhostLayers}" + """) >> 1) + firstSrc.z + """ + s"${Knowledge.numGhostLayers}" + """) * dzS
					+ (((y - firstDest.y - """ + s"${Knowledge.numGhostLayers}" + """) >> 1) + firstSrc.y + """ + s"${Knowledge.numGhostLayers}" + """) * dyS
					+ firstSrc.x + """ + s"${Knowledge.numGhostLayers}" + """;
				for (unsigned int x = firstDest.x + """ + s"${Knowledge.numGhostLayers}" + """; x <= lastDest.x - """ + s"${Knowledge.numGhostLayers}" + """; x += 2, destPos += 2, ++srcPos)
				{
					destData[destPos] += 1.0 * (
						srcData[srcPos]);

					if (x < lastDest.x - """ + s"${Knowledge.numGhostLayers}" + """)
						destData[destPos + dxD] += 0.5 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]);

					if (y < lastDest.y - """ + s"${Knowledge.numGhostLayers}" + """)
						destData[destPos + dyD] += 0.5 * (
						srcData[srcPos]
					+ srcData[srcPos + dyS]);

					if (z < lastDest.z - """ + s"${Knowledge.numGhostLayers}" + """)
						destData[destPos + dzD] += 0.5 * (
						srcData[srcPos]
					+ srcData[srcPos + dzS]);

					if (x < lastDest.x - """ + s"${Knowledge.numGhostLayers}" + """ && y < lastDest.y - """ + s"${Knowledge.numGhostLayers}" + """)
						destData[destPos + dyD + dxD] += 0.25 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]
					+ srcData[srcPos + dyS]
					+ srcData[srcPos + dyS + dxS]);

					if (x < lastDest.x - """ + s"${Knowledge.numGhostLayers}" + """ && z < lastDest.z - """ + s"${Knowledge.numGhostLayers}" + """)
						destData[destPos + dzD + dxD] += 0.25 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]
					+ srcData[srcPos + dzS]
					+ srcData[srcPos + dzS + dxS]);

					if (y < lastDest.y - """ + s"${Knowledge.numGhostLayers}" + """ && z < lastDest.z - """ + s"${Knowledge.numGhostLayers}" + """)
						destData[destPos + dzD + dyD] += 0.25 * (
						srcData[srcPos]
					+ srcData[srcPos + dyS]
					+ srcData[srcPos + dzS]
					+ srcData[srcPos + dzS + dyS]);

					if (x < lastDest.x - """ + s"${Knowledge.numGhostLayers}" + """ && y < lastDest.y - """ + s"${Knowledge.numGhostLayers}" + """ && z < lastDest.z - """ + s"${Knowledge.numGhostLayers}" + """)
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
""")));
  }
}

case class PerformCGS(level : Integer) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformVCycle\n";

  override def expand : ForLoopStatement = {
    Knowledge.cgs match {
      case CoarseGridSolverType.IP_Smoother =>
        ForLoopStatement(s"unsigned int s = 0", s"s < ${Knowledge.cgsNumSteps}", s"++s", ListBuffer(
          s"++solSlots[0];",
          s"performSmoothing_$level(fragments, (solSlots[$level] + 0) % ${Knowledge.numSolSlots}, (solSlots[$level] - 1) % ${Knowledge.numSolSlots});"))
    }
  }
}

case class PerformVCycle(level : Integer) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformVCycle\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performVCycle_$level", ListBuffer(Variable(s"Fragment3DCube*", s"fragments[${Knowledge.numFragsPerBlock}]"), Variable("unsigned int*", "solSlots")),
      (if (0 == level) {
        ListBuffer[Statement](new PerformCGS(level))
      } else {
        ListBuffer[Statement](
          ForLoopStatement(s"unsigned int s = 0", s"s < ${Knowledge.smootherNumPre}", s"++s", ListBuffer(
            s"++solSlots[$level];",
            s"performSmoothing_$level(fragments, (solSlots[$level] + 0) % ${Knowledge.numSolSlots}, (solSlots[$level] - 1) % ${Knowledge.numSolSlots});")),

          s"updateResidual_$level(fragments, solSlots[$level] % ${Knowledge.numSolSlots});",

          s"performRestriction_NodeFW(fragments, $level, ${level - 1});",

          s"setSolZero_${level - 1}(fragments, solSlots[${level - 1}] % ${Knowledge.numSolSlots});",

          s"performVCycle_${level - 1}(fragments, solSlots);",

          s"performProlongation_NodeTLI(fragments, solSlots[${level - 1}] % ${Knowledge.numSolSlots}, ${level - 1}, solSlots[$level] % ${Knowledge.numSolSlots}, $level);",

          ForLoopStatement(s"unsigned int s = 0", s"s < ${Knowledge.smootherNumPost}", s"++s", ListBuffer(
            s"++solSlots[$level];",
            s"performSmoothing_$level(fragments, (solSlots[$level] + 0) % ${Knowledge.numSolSlots}, (solSlots[$level] - 1) % ${Knowledge.numSolSlots});")))
      })
        ++
        (if (Knowledge.maxLevel == level) { ListBuffer[Statement](s"updateResidual_$level(fragments, solSlots[$level] % ${Knowledge.numSolSlots});") }
        else { ListBuffer[Statement]() }));
  }
}

case class GetGlobalResidual(field : Field) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = GetGlobalResidual\n";

  override def expand : FunctionStatement = {
    FunctionStatement("double", s"getGlobalResidual", ListBuffer(Variable(s"Fragment3DCube*", s"fragments[${Knowledge.numFragsPerBlock}]")),
      ListBuffer(
        s"double res = 0.0;",
        s"double resTotal = 0.0;",
        LoopOverFragments(ListBuffer(
          // FIXME: this currently counts duplicated values multiple times
          new LoopOverDimensions(
            fieldToIndexInner(Array(0, 0, 0), Knowledge.maxLevel), ListBuffer[Statement](
              s"double tmpRes =" ~ new FieldAccess(field, Knowledge.maxLevel, NumericLiteral(0), Mapping.access(Knowledge.maxLevel)) ~ ";",
              s"res += tmpRes * tmpRes;"))),
          true, "reduction(+:res)"),
        new MPI_Allreduce("&res", "&resTotal", NumericLiteral(1), "MPI_SUM"),
        s"return sqrt(resTotal);"));
  }
}

case class SetSolZero(field : Field, level : Integer) extends AbstractFunctionStatement with Expandable {
  // TODO: resolve/inline this function
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = SetSolZero\n";

  override def expand : FunctionStatement = {
    new FunctionStatement(new UnitDatatype(), s"setSolZero_$level", ListBuffer(Variable(s"Fragment3DCube*", s"fragments[${Knowledge.numFragsPerBlock}]"), Variable("unsigned int", "slot")),
      LoopOverFragments(ListBuffer(
        new LoopOverDimensions(fieldToIndexInner(Array(0, 0, 0), level),
          new AssignmentStatement(
            new FieldAccess(field, level, "slot", Mapping.access(level)),
            NumericLiteral(0.0))))));
  }
}
