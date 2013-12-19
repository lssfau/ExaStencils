package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.knowledge._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class PerformSmoothing() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = Smoother\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"smootherIteration_Node", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "targetSlot"), Variable("unsigned int", "sourceSlot"), Variable("unsigned int", "level")),
      ListBuffer(
        """
	exa_real_t h = 1.0;// / (1u << level);
	exa_real_t hSq = h * h;
	exa_real_t hSqInv = 1.0 / hSq;

	communicateSolution(fragments, level, sourceSlot);

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
			communicateSolution(fragments, level, targetSlot);
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

case class UpdateResidual() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = UpdateResidual\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"updateResidual_Node", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "slot"), Variable("unsigned int", "level")),
      ListBuffer(
        """
	exa_real_t h = 1.0;// / (1u << level);
	exa_real_t hSq = h * h;
	exa_real_t hSqInv = 1.0 / hSq;

	communicateSolution(fragments, level, slot);

#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
	for (int f = 0; f < fragments.size(); ++f)	// unsigned wont work... really, OMP?
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		const exa_real_t*	solData	= fragments[f]->solData[slot][level]->getDataPtr();
		exa_real_t*			resData	= fragments[f]->resData[0][level]->getDataPtr();
		const exa_real_t*	rhsData	= fragments[f]->rhsData[0][level]->getDataPtr();
		Vec3u				dimWPad	= fragments[f]->solData[0][level]->numDataPointsPerDimWPad;
		Vec3u				first	= fragments[f]->solData[0][level]->firstDataPoint;
		Vec3u				last	= fragments[f]->solData[0][level]->lastDataPoint;

		for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
			for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS;
				for (unsigned int x = first.x + NUM_GHOST_LAYERS; x <= last.x - NUM_GHOST_LAYERS; ++x, ++posAbs)
					resData[posAbs] =
					rhsData[posAbs]
				- solData[posAbs - 1] - solData[posAbs + 1]
				- solData[posAbs - dimWPad.x] - solData[posAbs + dimWPad.x]
				- solData[posAbs - dimWPad.y * dimWPad.x] - solData[posAbs + dimWPad.y * dimWPad.x]
				+ 6.0 * solData[posAbs];
			}
	}
"""));
  }
}

case class PerformRestriction() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformRestriction\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performRestriction_NodeFW", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "levelSrc"), Variable("unsigned int", "levelDest")),
      ListBuffer(
        """
	communicateResidual(fragments, levelSrc);

#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
	for (int f = 0; f < fragments.size(); ++f)
	{
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
	}
"""));
  }
}

case class PerformProlongation() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformProlongation\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performProlongation_NodeTLI", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "slotSrc"), Variable("unsigned int", "levelSrc"), Variable("unsigned int", "slotDest"), Variable("unsigned int", "levelDest")),
      ListBuffer(
        """
	communicateSolution(fragments, levelSrc, slotSrc);

#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
	for (int f = 0; f < fragments.size(); ++f)
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
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
					// TODO: extract if condition

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
	}
"""));
  }
}

case class PerformVCycle() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PerformVCycle\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"performVCycle", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "level"), Variable("unsigned int*", "solSlots")),
      ListBuffer(
        """
#ifdef MEASURE_MG_COMPONENTS
	StopWatch	stopWatch;
#endif

	// check for coarsest level
	if (COARSE_LEVEL == level)
	{
#ifdef COARSE_GRID_SOLVER_IP_SMOOTHER
		for (unsigned int s = 0; s < NUM_COARSE_STEPS; ++s)
		{
			++solSlots[COARSE_LEVEL];
			smootherIteration_Node(fragments, (solSlots[level] + 0) % NUM_SOL_SLOTS, (solSlots[level] - 1) % NUM_SOL_SLOTS, level);
		}
#endif

#ifdef MEASURE_MG_COMPONENTS
		if (0 == mpiRank)
			LOG_NOTE("Coarse grid solving on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

		if (FINAL_LEVEL == COARSE_LEVEL)	// no real MG, just simple Jacobi
			updateResidual_Node(fragments, solSlots[level] % NUM_SOL_SLOTS, level);

		return;
	}

	// not coarsest level, perform default MG step

#ifdef MEASURE_MG_COMPONENTS
	stopWatch.reset();
#endif

	// pre-smoothing
	for (unsigned int s = 0; s < NUM_PRE_SMOOTHING_STEPS; ++s)
	{
		++solSlots[level];
		smootherIteration_Node(fragments, (solSlots[level] + 0) % NUM_SOL_SLOTS, (solSlots[level] - 1) % NUM_SOL_SLOTS, level);
	}

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Pre-Smoothing on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// residual calculation
	updateResidual_Node(fragments, solSlots[level] % NUM_SOL_SLOTS, level);

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Updating the residual on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// restriction
	performRestriction_NodeFW(fragments, level, level - 1);

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Restricting on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// reset solution of coarser grid
	setSolZero_Node(fragments, level - 1, solSlots[level - 1] % NUM_SOL_SLOTS);

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Resetting the solution on level " << level - 1 << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// solve on coarse grid
	performVCycle(fragments, level - 1, solSlots);

#ifdef MEASURE_MG_COMPONENTS
	stopWatch.reset();
#endif

	// prolongation & correction
	performProlongation_NodeTLI(fragments, solSlots[level - 1] % NUM_SOL_SLOTS, level - 1, solSlots[level] % NUM_SOL_SLOTS, level);

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Prolongation on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// post-smoothing
	for (unsigned int s = 0; s < NUM_POST_SMOOTHING_STEPS; ++s)
	{
		++solSlots[level];
		smootherIteration_Node(fragments, (solSlots[level] + 0) % NUM_SOL_SLOTS, (solSlots[level] - 1) % NUM_SOL_SLOTS, level);
	}

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Post-Smoothing on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// prepare res for reduction on finest level
	if (FINAL_LEVEL == level)
		updateResidual_Node(fragments, solSlots[level] % NUM_SOL_SLOTS, level);
"""));
  }
}

case class GetGlobalResidual() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = GetGlobalResidual\n";

  override def expand : FunctionStatement = {
    FunctionStatement("exa_real_t", s"getGlobalResidual_Node", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", " level")),
      ListBuffer(
        """
	exa_real_t res = 0.0;

#ifdef USE_OMP
#	pragma omp parallel for reduction(+:res) schedule(static, 1)
#endif
	for (int f = 0; f < fragments.size(); ++f)
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		const exa_real_t*	data	= fragments[f]->resData[0][level]->getDataPtr();
		Vec3u				dimWPad	= fragments[f]->solData[0][level]->numDataPointsPerDimWPad;
		Vec3u				first	= fragments[f]->solData[0][level]->firstDataPoint;
		Vec3u				last	= fragments[f]->solData[0][level]->lastDataPoint;

		for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
		{
			for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS;
				for (unsigned int x = first.x + NUM_GHOST_LAYERS; x <= last.x - NUM_GHOST_LAYERS; ++x, ++posAbs)
					res += data[posAbs] * data[posAbs];
			}
		}
	}

	return sqrt(res);
"""));
  }
}

case class SetSolZero() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = SetSolZero\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"setSolZero_Node", ListBuffer(Variable("std::vector<Fragment3DCube*>&", "fragments"), Variable("unsigned int", "level"), Variable("unsigned int", "slot")),
      ListBuffer(
        """
#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
	for (int e = 0; e < fragments.size(); ++e)
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		exa_real_t*			data	= fragments[e]->solData[slot][level]->getDataPtr();
		Vec3u				dimWPad	= fragments[e]->solData[0][level]->numDataPointsPerDimWPad;
		Vec3u				first	= fragments[e]->solData[0][level]->firstDataPoint;
		Vec3u				last	= fragments[e]->solData[0][level]->lastDataPoint;

		for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
		{
			for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS;
				for (unsigned int x = first.x + NUM_GHOST_LAYERS; x <= last.x - NUM_GHOST_LAYERS; ++x, ++posAbs)
					data[posAbs] = 0.0;
			}
		}
	}
"""));
  }
}
