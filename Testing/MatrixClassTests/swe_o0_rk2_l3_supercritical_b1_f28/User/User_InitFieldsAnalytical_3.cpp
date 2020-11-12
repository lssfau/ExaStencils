#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void InitFieldsAnalytical_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_InitFieldsAnalytical;
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i00_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
					fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
					fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
					fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
				}
			}
		}
	}
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_InitFieldsAnalytical;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
