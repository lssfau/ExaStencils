#include "Domain/Domain.h"

void initDomain () {
	if ((mpiSize!=1)) {
		if ((0==mpiRank)) {
			std::cout << "Invalid number of MPI processes (" << " " << mpiSize << " " << ") should be " << " " << 1 << " " << std::endl;
		}
		exit(1);
	}
	for (int fragmentIdx = 0; fragmentIdx<1; ++fragmentIdx) {
		fragmentPos_0 = (0.1*((mpiRank%1)+(fragmentIdx%1)+0.5));
		fragmentPos_1 = (0.1*((mpiRank%1)+(fragmentIdx%1)+0.5));
		fragmentIndex_0 = ((int)floor((10.0*fragmentPos_0)));
		fragmentIndex_1 = ((int)floor((10.0*fragmentPos_1)));
		fragmentId = (((int)floor((10.0*fragmentPos_0)))+((int)floor((10.0*fragmentPos_1))));
		commId = ((((int)floor((10.0*fragmentPos_0)))%1)+(((int)floor((10.0*fragmentPos_1)))%1));
		fragmentPosBegin_0 = (fragmentPos_0-0.05);
		fragmentPosBegin_1 = (fragmentPos_1-0.05);
		fragmentPosEnd_0 = (fragmentPos_0+0.05);
		fragmentPosEnd_1 = (fragmentPos_1+0.05);
	}
	for (int fragmentIdx = 0; fragmentIdx<1; ++fragmentIdx) {
		
	}
	/* -.-.-.- inlined setupBuffers -.-.-.- */
	/* =^=^=^=^= end setupBuffers =^=^=^=^= */
}
