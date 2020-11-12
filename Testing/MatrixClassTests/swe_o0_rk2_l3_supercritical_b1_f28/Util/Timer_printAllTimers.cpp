#include "fstream"
#include "sstream"
#include "iomanip"

#include "Util/Timer.h"

void printAllTimers () {
	{
		double timerValue = getTotalTime(timer_AdvanceRK2);
		std::cout << "Mean mean total time for Timer AdvanceRK2:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_ApplyAllBc);
		std::cout << "Mean mean total time for Timer ApplyAllBc:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_CommunicateAll);
		std::cout << "Mean mean total time for Timer CommunicateAll:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_InitFieldsAnalytical);
		std::cout << "Mean mean total time for Timer InitFieldsAnalytical:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_MinDepth);
		std::cout << "Mean mean total time for Timer MinDepth:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_PrintDebugFields);
		std::cout << "Mean mean total time for Timer PrintDebugFields:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_SetupPhase);
		std::cout << "Mean mean total time for Timer SetupPhase:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_SolvePhase);
		std::cout << "Mean mean total time for Timer SolvePhase:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_UpdateCTilde);
		std::cout << "Mean mean total time for Timer UpdateCTilde:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_UpdateUnknowns);
		std::cout << "Mean mean total time for Timer UpdateUnknowns:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_print);
		std::cout << "Mean mean total time for Timer print:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_setup);
		std::cout << "Mean mean total time for Timer setup:" << " " << timerValue << " " << std::endl;
	}
	{
		double timerValue = getTotalTime(timer_timeLoop);
		std::cout << "Mean mean total time for Timer timeLoop:" << " " << timerValue << " " << std::endl;
	}
}
