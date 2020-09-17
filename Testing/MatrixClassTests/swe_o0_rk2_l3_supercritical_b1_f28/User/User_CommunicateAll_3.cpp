#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void CommunicateAll_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_CommunicateAll;
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i00_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	exchcetaLower0_all_a_a_a_a_3(0);
	exchcetaUpper0_all_a_a_a_a_3(0);
	exchcuLower0_all_a_a_a_a_3(0);
	exchcuUpper0_all_a_a_a_a_3(0);
	exchcvLower0_all_a_a_a_a_3(0);
	exchcvUpper0_all_a_a_a_a_3(0);
	exchcuTildeLower0_all_a_a_a_a_3(0);
	exchcuTildeUpper0_all_a_a_a_a_3(0);
	exchcvTildeLower0_all_a_a_a_a_3(0);
	exchcvTildeUpper0_all_a_a_a_a_3(0);
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_CommunicateAll;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
