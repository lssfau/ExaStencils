#ifndef EXASTENCILS_UTIL_TIMER_H
#define EXASTENCILS_UTIL_TIMER_H

#include "fstream"
#include "iostream"

#include "fstream"
#include "sstream"
#include "iomanip"

#include "Global/Global.h"
#include "Util/Stopwatch.h"

double getTotalTime(StopWatch& stopWatch);
void printAllTimers();
void printAllTimersToFile();
void reduceTimers();

#endif
