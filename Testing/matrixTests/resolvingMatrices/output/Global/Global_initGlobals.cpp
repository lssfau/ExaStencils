#include "cstdlib"

#include "Global/Global.h"

void initGlobals () {
	mpiCommunicator = MPI_COMM_WORLD;
	MPI_Comm_rank(mpiCommunicator, &mpiRank);
	MPI_Comm_size(mpiCommunicator, &mpiSize);
	std::srand(mpiRank);
	mat[0] = 1.0;
	mat[1] = 2.0;
	mat[2] = 3.0;
	mat[3] = 4.0;
	{
		commId = -1;
	}
	{
		fragmentId = -1;
	}
	{
		fragmentIndex_0 = 0;
	}
	{
		fragmentIndex_1 = 0;
	}
	{
		fragmentPosBegin_0 = 0.0;
	}
	{
		fragmentPosBegin_1 = 0.0;
	}
	{
		fragmentPosEnd_0 = 0.0;
	}
	{
		fragmentPosEnd_1 = 0.0;
	}
	{
		fragmentPos_0 = 0.0;
	}
	{
		fragmentPos_1 = 0.0;
	}
}
