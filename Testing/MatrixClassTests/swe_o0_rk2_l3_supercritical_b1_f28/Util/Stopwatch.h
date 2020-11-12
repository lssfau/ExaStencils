#ifndef EXASTENCILS_UTIL_STOPWATCH_H
#define EXASTENCILS_UTIL_STOPWATCH_H

#include "string"
#include "chrono"

#include "fstream"
#include "sstream"
#include "iomanip"

class StopWatch {
	public:
		StopWatch()
			: numEntries(0), numMeasurements(0), lastTimeMeasured(std::chrono::nanoseconds::zero()), totalTimeMeasured(std::chrono::nanoseconds::zero()), totalTimeAveraged(0.0)
		{}
	
		std::string timerName;
	
		int numEntries;
		int numMeasurements;
		std::chrono::high_resolution_clock::time_point timerStarted;
		std::chrono::high_resolution_clock::time_point timerEnded;
		std::chrono::nanoseconds lastTimeMeasured;
		std::chrono::nanoseconds totalTimeMeasured;
		double totalTimeAveraged;
};


#endif
