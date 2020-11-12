#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void SolvePhase_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i01_stopWatch = timer_SolvePhase;
	if ((0==_i01_stopWatch.numEntries)) {
		
		_i01_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i01_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i01_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	while ((it<2000)) {
		/* -.-.-.- inlined startTimer -.-.-.- */
		StopWatch& _i00_stopWatch = timer_timeLoop;
		if ((0==_i00_stopWatch.numEntries)) {
			
			_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
			_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
		}
		(++_i00_stopWatch.numEntries);
		/* =^=^=^=^= end startTimer =^=^=^=^= */
		AdvanceRK2_3();
		it += 1;
		/* -.-.-.- inlined stopTimer -.-.-.- */
		StopWatch& stopWatch = timer_timeLoop;
		(--stopWatch.numEntries);
		if ((0==stopWatch.numEntries)) {
			
			stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
			stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
			stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
			(++stopWatch.numMeasurements);
		}
		/* =^=^=^=^= end stopTimer =^=^=^=^= */
		if (((fmod(t, 200)<(0.1*dt))||(fmod(t, 200)>(200-(0.1*dt))))) {
			/* -.-.-.- inlined startTimer -.-.-.- */
			StopWatch& _i00_stopWatch = timer_print;
			if ((0==_i00_stopWatch.numEntries)) {
				
				_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
				_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
			}
			(++_i00_stopWatch.numEntries);
			/* =^=^=^=^= end startTimer =^=^=^=^= */
			PrintDebugFields_3();
			/* -.-.-.- inlined stopTimer -.-.-.- */
			StopWatch& stopWatch = timer_print;
			(--stopWatch.numEntries);
			if ((0==stopWatch.numEntries)) {
				
				stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
				stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
				stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
				(++stopWatch.numMeasurements);
			}
			/* =^=^=^=^= end stopTimer =^=^=^=^= */
			print_count += 1;
		}
	}
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_SolvePhase;
	(--_i00_stopWatch.numEntries);
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((_i00_stopWatch.timerEnded-_i00_stopWatch.timerStarted));
		_i00_stopWatch.totalTimeMeasured += _i00_stopWatch.lastTimeMeasured;
		(++_i00_stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
