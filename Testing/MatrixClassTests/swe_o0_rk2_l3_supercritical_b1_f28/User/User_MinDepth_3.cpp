#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void MinDepth_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_MinDepth;
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
					if (((((fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+(1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]))<0.05)||((fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+(1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]))<0.05))||((fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+(1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]))<0.05))) {
						std::cout << "water is too shallow at (" << " " << ((0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)])+(0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)])+(0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)])) << " " << "," << " " << ((0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)])+(0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)])+(0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)])) << " " << ")" << " " << std::endl;
						fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(0.05-std::min({fieldData_bath[fragmentIdx][((11*i1)+i0+12)],std::min({fieldData_bath[fragmentIdx][((11*i1)+i0+23)],fieldData_bath[fragmentIdx][((11*i1)+i0+13)]})}))*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
						fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
					}
					if (((((fieldData_bath[fragmentIdx][((11*i1)+i0+24)]+(1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]))<0.05)||((fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+(1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]))<0.05))||((fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+(1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]))<0.05))) {
						std::cout << "water is too shallow at (" << " " << ((0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)])+(0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)])+(0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << " " << "," << " " << ((0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)])+(0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)])+(0.333333333333333*fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << " " << ")" << " " << std::endl;
						fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(0.05-std::min({fieldData_bath[fragmentIdx][((11*i1)+i0+23)],std::min({fieldData_bath[fragmentIdx][((11*i1)+i0+13)],fieldData_bath[fragmentIdx][((11*i1)+i0+24)]})}))*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
						fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
					}
				}
			}
		}
	}
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_MinDepth;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
