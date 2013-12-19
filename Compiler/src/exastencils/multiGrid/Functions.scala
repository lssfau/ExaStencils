package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.core._

import exastencils.knowledge._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives._

case class PerformSmoothingJacobi(solutionField : Field, rhsField : Field, level : Integer) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformSmoothingJacobi\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performSmoothing_$level", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "targetSlot"), Variable("unsigned int", "sourceSlot")),
      ListBuffer(
        s"exchsolData(fragments, $level, sourceSlot);",
        new LoopOverFragments(
          new LoopOverDimensions(
            fieldToIndexInner(Array(0, 0, 0), level), ListBuffer[Statement](
              AssignmentStatement(
                // FIXME: introduce and apply stencil node
                FieldAccess(solutionField, level, "targetSlot", Mapping.access(level)),
                s"(1.0 - OMEGA) * " ~ FieldAccess(solutionField, level, "sourceSlot", Mapping.access(level))
                  ~ s"+ OMEGA * 1.0 / 6.0 * ("
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

case class PerformSmoothing() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformSmoothing\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"smootherIteration_Node", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "targetSlot"), Variable("unsigned int", "sourceSlot"), Variable("unsigned int", "level")),
      ListBuffer(
        """
	exa_real_t h = 1.0;// / (1u << level);
	exa_real_t hSq = h * h;
	exa_real_t hSqInv = 1.0 / hSq;

	exchsolData(fragments, level, sourceSlot);

#ifdef SMOOTHER_GSOD
	for (unsigned int smootherIteration = 0; smootherIteration < NUM_GSOD_ITERATIONS; ++smootherIteration)
	{
#endif

		// solver iteration (elements)
#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
		for (int f = 0; f < fragments.size(); ++f)	// unsigned wont work... really, OMP?
		{
			// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
			const exa_real_t*	solSrcData	= fragments[f]->solData[sourceSlot][level]->getDataPtr();
			exa_real_t*			solDestData	= fragments[f]->solData[targetSlot][level]->getDataPtr();
			const exa_real_t*	rhsData		= fragments[f]->rhsData[0][level]->getDataPtr();
			Vec3u				dimWOPad	= fragments[f]->solData[0][level]->numDataPointsPerDim;
			Vec3u				dimWPad		= fragments[f]->solData[0][level]->numDataPointsPerDimWPad;
			Vec3u				first		= fragments[f]->solData[0][level]->firstDataPoint;
			Vec3u				last		= fragments[f]->solData[0][level]->lastDataPoint;

#ifdef SMOOTHER_GSACBE
			Vec3u				numBlocks	= (dimWOPad - Vec3u(2 * NUM_GHOST_LAYERS) + Vec3u(GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP - 1)) / Vec3u(GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP) + Vec3u(1);

			for (unsigned int bz = 0; bz < numBlocks.z; ++bz)
				for (unsigned int by = 0; by < numBlocks.y; ++by)
					for (unsigned int bx = 0; bx < numBlocks.x; ++bx)
					{
						unsigned int zMin = NUM_GHOST_LAYERS + bz * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
						unsigned int zMax = std::min(zMin + GSBE_WINDOW_SIZE, dimWOPad.z - NUM_GHOST_LAYERS);
						unsigned int yMin = NUM_GHOST_LAYERS + by * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
						unsigned int yMax = std::min(yMin + GSBE_WINDOW_SIZE, dimWOPad.y - NUM_GHOST_LAYERS);
						unsigned int xMin = NUM_GHOST_LAYERS + bx * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
						unsigned int xMax = std::min(xMin + GSBE_WINDOW_SIZE, dimWOPad.x - NUM_GHOST_LAYERS);

						for (unsigned int i = 0; i < NUM_GSBE_ITERATIONS; ++i)
						{
							for (unsigned int z = zMin + first.z; z < zMax + first.z; ++z)
								for (unsigned int y = yMin + first.y; y < yMax + first.y; ++y)
									for (unsigned int x = xMin + first.x; x < xMax + first.x; ++x)
									{
										unsigned int posAbs = z * dimWPad.xy().componentProd() + y * dimWPad.x + x;
										solDestData[posAbs] = 
											(1.0 - OMEGA) * solSrcData[posAbs]
										+ OMEGA * 1.0 / 6.0 * (
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
					(1.0 - OMEGA) * solSrcData[posAbs]
				+ OMEGA * 0.25 * (solSrcData[posAbs - 1] + solSrcData[posAbs + 1] + solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x] - rhsData[posAbs]);
			}
#elif defined (SMOOTHER_GSRB) || defined (SMOOTHER_GSRBAC)
			for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
			{
				for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
				{
					unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS + ((y - first.y - NUM_GHOST_LAYERS) % 2);
					for (unsigned int x = first.x + NUM_GHOST_LAYERS + ((y - first.y - NUM_GHOST_LAYERS) % 2); x <= last.x - NUM_GHOST_LAYERS; x += 2, posAbs += 2)
					{
						solDestData[posAbs] = 
							(1.0 - OMEGA) * solSrcData[posAbs]
						+ OMEGA * 1.0 / 6.0 * (
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

			for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
			{
				for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
				{
					unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS + ((y - first.y - NUM_GHOST_LAYERS + 1) % 2);
					for (unsigned int x = first.x + NUM_GHOST_LAYERS + ((y - first.y - NUM_GHOST_LAYERS + 1) % 2); x <= last.x - NUM_GHOST_LAYERS; x += 2, posAbs += 2)
					{
						solDestData[posAbs] = 
							(1.0 - OMEGA) * solSrcData[posAbs]
						+ OMEGA * 1.0 / 6.0 * (
							solSrcData[posAbs - 1] + solSrcData[posAbs + 1]
						+ solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x]
						+ solSrcData[posAbs - dimWPad.x * dimWPad.y] + solSrcData[posAbs + dimWPad.x * dimWPad.y]
						- rhsData[posAbs]);
					}
				}
			}
#else
			for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
			{
				for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
				{
					unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS;
					for (unsigned int x = first.x + NUM_GHOST_LAYERS; x <= last.x - NUM_GHOST_LAYERS; ++x, ++posAbs)
					{
						solDestData[posAbs] = 
							(1.0 - OMEGA) * solSrcData[posAbs]
						+ OMEGA * 1.0 / 6.0 * (
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
}

case class UpdateResidual(residualField : Field, solutionField : Field, rhsField : Field, level : Integer) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = UpdateResidual\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"updateResidual_$level", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "slot")),
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
    FunctionStatement(new UnitDatatype(), s"performRestriction_NodeFW", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "levelSrc"), Variable("unsigned int", "levelDest")),
      ListBuffer(
        s"exchresData(fragments, levelSrc, 0);",
        new LoopOverFragments(
          """
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		const exa_real_t*	srcData		= fragments[f]->resData[0][levelSrc]->getDataPtr();
		exa_real_t*			destData	= fragments[f]->rhsData[0][levelDest]->getDataPtr();
		Vec3u				dimWPadSrc	= fragments[f]->solData[0][levelSrc]->numDataPointsPerDimWPad;
		Vec3u				firstSrc	= fragments[f]->solData[0][levelSrc]->firstDataPoint;
		Vec3u				lastSrc		= fragments[f]->solData[0][levelSrc]->lastDataPoint;
		Vec3u				dimWPadDest	= fragments[f]->solData[0][levelDest]->numDataPointsPerDimWPad;
		Vec3u				firstDest	= fragments[f]->solData[0][levelDest]->firstDataPoint;
		Vec3u				lastDest	= fragments[f]->solData[0][levelDest]->lastDataPoint;

		unsigned int		dxS = 1;
		unsigned int		dyS	= dimWPadSrc.x;
		unsigned int		dzS	= dimWPadSrc.x * dimWPadSrc.y;

		for (unsigned int z = firstDest.z + NUM_GHOST_LAYERS; z <= lastDest.z - NUM_GHOST_LAYERS; ++z)
		{
			for (unsigned int y = firstDest.y + NUM_GHOST_LAYERS; y <= lastDest.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = (2 * (z - firstDest.z - NUM_GHOST_LAYERS) + firstSrc.z + NUM_GHOST_LAYERS) * dzS
					+ (2 * (y - firstDest.y - NUM_GHOST_LAYERS) + firstSrc.y + NUM_GHOST_LAYERS) * dyS
					+ firstSrc.x + NUM_GHOST_LAYERS;
				for (unsigned int x = firstDest.x + NUM_GHOST_LAYERS; x <= lastDest.x - NUM_GHOST_LAYERS; ++x, posAbs += 2)
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
    FunctionStatement(new UnitDatatype(), s"performProlongation_NodeTLI", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "slotSrc"), Variable("unsigned int", "levelSrc"), Variable("unsigned int", "slotDest"), Variable("unsigned int", "levelDest")),
      ListBuffer(
        s"exchsolData(fragments, levelSrc, slotSrc);",
        new LoopOverFragments(
          """
		// WARNING: assumes an odd number of data points per dimension (e.g. 2^l + 1) or a number smaller or equal to 2
		const exa_real_t*	srcData		= fragments[f]->solData[slotSrc][levelSrc]->getDataPtr();
		exa_real_t*			destData	= fragments[f]->solData[slotDest][levelDest]->getDataPtr();
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

		for (unsigned int z = firstDest.z + NUM_GHOST_LAYERS; z <= lastDest.z - NUM_GHOST_LAYERS; z += 2)
		{
			for (unsigned int y = firstDest.y + NUM_GHOST_LAYERS; y <= lastDest.y - NUM_GHOST_LAYERS; y += 2)
			{
				unsigned int destPos = z * dzD + y * dyD + firstDest.x + NUM_GHOST_LAYERS;
				unsigned int srcPos = (((z - firstDest.z - NUM_GHOST_LAYERS) >> 1) + firstSrc.z + NUM_GHOST_LAYERS) * dzS
					+ (((y - firstDest.y - NUM_GHOST_LAYERS) >> 1) + firstSrc.y + NUM_GHOST_LAYERS) * dyS
					+ firstSrc.x + NUM_GHOST_LAYERS;
				for (unsigned int x = firstDest.x + NUM_GHOST_LAYERS; x <= lastDest.x - NUM_GHOST_LAYERS; x += 2, destPos += 2, ++srcPos)
				{
					destData[destPos] += 1.0 * (
						srcData[srcPos]);

					if (x < lastDest.x - NUM_GHOST_LAYERS)
						destData[destPos + dxD] += 0.5 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]);

					if (y < lastDest.y - NUM_GHOST_LAYERS)
						destData[destPos + dyD] += 0.5 * (
						srcData[srcPos]
					+ srcData[srcPos + dyS]);

					if (z < lastDest.z - NUM_GHOST_LAYERS)
						destData[destPos + dzD] += 0.5 * (
						srcData[srcPos]
					+ srcData[srcPos + dzS]);

					if (x < lastDest.x - NUM_GHOST_LAYERS && y < lastDest.y - NUM_GHOST_LAYERS)
						destData[destPos + dyD + dxD] += 0.25 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]
					+ srcData[srcPos + dyS]
					+ srcData[srcPos + dyS + dxS]);

					if (x < lastDest.x - NUM_GHOST_LAYERS && z < lastDest.z - NUM_GHOST_LAYERS)
						destData[destPos + dzD + dxD] += 0.25 * (
						srcData[srcPos]
					+ srcData[srcPos + dxS]
					+ srcData[srcPos + dzS]
					+ srcData[srcPos + dzS + dxS]);

					if (y < lastDest.y - NUM_GHOST_LAYERS && z < lastDest.z - NUM_GHOST_LAYERS)
						destData[destPos + dzD + dyD] += 0.25 * (
						srcData[srcPos]
					+ srcData[srcPos + dyS]
					+ srcData[srcPos + dzS]
					+ srcData[srcPos + dzS + dyS]);

					if (x < lastDest.x - NUM_GHOST_LAYERS && y < lastDest.y - NUM_GHOST_LAYERS && z < lastDest.z - NUM_GHOST_LAYERS)
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

case class PerformVCycle(fields : FieldCollection /*TODO: check if necessary*/ , level : Integer) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformVCycle\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performVCycle_$level", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int*", "solSlots")),
      (if (0 == level) {
        ListBuffer[Statement](
            // TODO: choice by enum
            // FIXME: extract CGS 
          s"#ifdef COARSE_GRID_SOLVER_IP_SMOOTHER",
          s"for (unsigned int s = 0; s < NUM_COARSE_STEPS; ++s) 		{",
          s"++solSlots[COARSE_LEVEL];",
          s"performSmoothing_$level(fragments, (solSlots[$level] + 0) % NUM_SOL_SLOTS, (solSlots[$level] - 1) % NUM_SOL_SLOTS);",
          s"}",
          s"#endif")
      } else {
        ListBuffer[Statement](
          s"for (unsigned int s = 0; s < NUM_PRE_SMOOTHING_STEPS; ++s)	{",
          s"++solSlots[$level];",
          s"performSmoothing_$level(fragments, (solSlots[$level] + 0) % NUM_SOL_SLOTS, (solSlots[$level] - 1) % NUM_SOL_SLOTS);",
          s"}",

          s"updateResidual_$level(fragments, solSlots[$level] % NUM_SOL_SLOTS);",

          s"performRestriction_NodeFW(fragments, $level, ${level - 1});",

          s"setSolZero_${level - 1}(fragments, solSlots[${level - 1}] % NUM_SOL_SLOTS);",

          s"performVCycle_${level - 1}(fragments, solSlots);",

          s"performProlongation_NodeTLI(fragments, solSlots[${level - 1}] % NUM_SOL_SLOTS, ${level - 1}, solSlots[$level] % NUM_SOL_SLOTS, $level);",

          s"for (unsigned int s = 0; s < NUM_POST_SMOOTHING_STEPS; ++s)	{",
          s"++solSlots[$level];",
          s"performSmoothing_$level(fragments, (solSlots[$level] + 0) % NUM_SOL_SLOTS, (solSlots[$level] - 1) % NUM_SOL_SLOTS);",
          s"}")
      })
        ++ (if (Knowledge.maxLevel == level) { ListBuffer[Statement](s"updateResidual_$level(fragments, solSlots[$level] % NUM_SOL_SLOTS);") }
        else { ListBuffer[Statement]() }));
  }
}

case class GetGlobalResidual(field : Field) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = GetGlobalResidual\n";

  override def expand : FunctionStatement = {
    FunctionStatement("exa_real_t", s"getGlobalResidual", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments")),
      ListBuffer(
        s"exa_real_t res = 0.0;",
        LoopOverFragments(ListBuffer(
          // FIXME: this currently counts duplicated values multiple times
          new LoopOverDimensions(
            fieldToIndexInner(Array(0, 0, 0), Knowledge.maxLevel), ListBuffer[Statement](
              s"exa_real_t tmpRes =" ~ new FieldAccess(field, Knowledge.maxLevel, NumericLiteral(0), Mapping.access(Knowledge.maxLevel)) ~ ";",
              s"res += tmpRes * tmpRes;"))),
          true, "reduction(+:res)"),
        s"return sqrt(res);"));
  }
}

case class SetSolZero(field : Field, level : Integer) extends AbstractFunctionStatement with Expandable {
  // TODO: resolve/inline this function
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = SetSolZero\n";

  override def expand : FunctionStatement = {
    new FunctionStatement(new UnitDatatype(), s"setSolZero_$level", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "slot")),
      LoopOverFragments(ListBuffer(
        new LoopOverDimensions(fieldToIndexInner(Array(0, 0, 0), level),
          new AssignmentStatement(
            new FieldAccess(field, level, "slot", Mapping.access(level)),
            ImplicitConversions.NumberToNumericLiteral(0.0))))));
  }
}
