#include "fstream"
#include "sstream"
#include "iomanip"

#include "Util/Timer.h"

double getTotalTime (StopWatch& stopWatch) {
	if ((0!=stopWatch.totalTimeAveraged)) {
		return stopWatch.totalTimeAveraged;
	} else {
		return (1.0E-6*std::chrono::duration_cast<std::chrono::nanoseconds>(stopWatch.totalTimeMeasured).count());
	}
}
