#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void AdvanceRK2_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_AdvanceRK2;
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i00_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	UpdateUnknowns_3();
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldData_cetaOldLower0[fragmentIdx][((10*i1)+i0+11)] = fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)];
					fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] += (dt*fieldData_cetaNewLower0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cuOldLower0[fragmentIdx][((10*i1)+i0+11)] = fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)];
					fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] += (dt*fieldData_cuNewLower0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvOldLower0[fragmentIdx][((10*i1)+i0+11)] = fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)];
					fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] += (dt*fieldData_cvNewLower0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cetaOldUpper0[fragmentIdx][((10*i1)+i0+11)] = fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)];
					fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] += (dt*fieldData_cetaNewUpper0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cuOldUpper0[fragmentIdx][((10*i1)+i0+11)] = fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)];
					fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] += (dt*fieldData_cuNewUpper0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvOldUpper0[fragmentIdx][((10*i1)+i0+11)] = fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)];
					fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] += (dt*fieldData_cvNewUpper0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
	}
	t += dt;
	UpdateCTilde_3();
	CommunicateAll_3();
	ApplyAllBc_3();
	UpdateUnknowns_3();
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = (0.5*((dt*fieldData_cetaNewLower0[fragmentIdx][((10*i1)+i0+11)])+fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cetaOldLower0[fragmentIdx][((10*i1)+i0+11)]));
					fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (0.5*((dt*fieldData_cuNewLower0[fragmentIdx][((10*i1)+i0+11)])+fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cuOldLower0[fragmentIdx][((10*i1)+i0+11)]));
					fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = (0.5*((dt*fieldData_cvNewLower0[fragmentIdx][((10*i1)+i0+11)])+fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cvOldLower0[fragmentIdx][((10*i1)+i0+11)]));
					fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.5*((dt*fieldData_cetaNewUpper0[fragmentIdx][((10*i1)+i0+11)])+fieldData_cetaOldUpper0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]));
					fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.5*((dt*fieldData_cuNewUpper0[fragmentIdx][((10*i1)+i0+11)])+fieldData_cuOldUpper0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)]));
					fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.5*((dt*fieldData_cvNewUpper0[fragmentIdx][((10*i1)+i0+11)])+fieldData_cvOldUpper0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)]));
				}
			}
		}
	}
	MinDepth_3();
	UpdateCTilde_3();
	CommunicateAll_3();
	ApplyAllBc_3();
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_AdvanceRK2;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
