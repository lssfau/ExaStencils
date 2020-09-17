#include "fstream"
#include "sstream"
#include "iomanip"

#include "Util/Timer.h"

void reduceTimers () {
	{
		timer_AdvanceRK2.totalTimeAveraged = getTotalTime(timer_AdvanceRK2);
	}
	{
		timer_ApplyAllBc.totalTimeAveraged = getTotalTime(timer_ApplyAllBc);
	}
	{
		timer_CommunicateAll.totalTimeAveraged = getTotalTime(timer_CommunicateAll);
	}
	{
		timer_InitFieldsAnalytical.totalTimeAveraged = getTotalTime(timer_InitFieldsAnalytical);
	}
	{
		timer_MinDepth.totalTimeAveraged = getTotalTime(timer_MinDepth);
	}
	{
		timer_PrintDebugFields.totalTimeAveraged = getTotalTime(timer_PrintDebugFields);
	}
	{
		timer_SetupPhase.totalTimeAveraged = getTotalTime(timer_SetupPhase);
	}
	{
		timer_SolvePhase.totalTimeAveraged = getTotalTime(timer_SolvePhase);
	}
	{
		timer_UpdateCTilde.totalTimeAveraged = getTotalTime(timer_UpdateCTilde);
	}
	{
		timer_UpdateUnknowns.totalTimeAveraged = getTotalTime(timer_UpdateUnknowns);
	}
	{
		timer_print.totalTimeAveraged = getTotalTime(timer_print);
	}
	{
		timer_setup.totalTimeAveraged = getTotalTime(timer_setup);
	}
	{
		timer_timeLoop.totalTimeAveraged = getTotalTime(timer_timeLoop);
	}
}
