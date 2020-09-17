#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

int main (int argc, char** argv) {
	initGlobals();
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_setup;
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i00_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	readParameterFile("../../swe_o0_rk2_l3_supercritical_b1_f28.parameter");
	initDomain();
	initGeometry();
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_setup;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
	SetupPhase_3();
	SolvePhase_3();
	reduceTimers();
	destroyGlobals();
	return 0;
}
