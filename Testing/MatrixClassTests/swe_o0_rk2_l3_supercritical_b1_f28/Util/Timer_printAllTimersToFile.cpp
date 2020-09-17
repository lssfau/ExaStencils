#include "fstream"
#include "sstream"
#include "iomanip"

#include "Util/Timer.h"

void printAllTimersToFile () {
	double timesToPrint[26];
	timesToPrint[0] = getTotalTime(timer_AdvanceRK2);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& stopWatch = timer_AdvanceRK2;
	timesToPrint[1] = ((0==stopWatch.numMeasurements) ? 0.0 : (getTotalTime(stopWatch)/stopWatch.numMeasurements));
	timesToPrint[2] = getTotalTime(timer_ApplyAllBc);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i00_stopWatch = timer_ApplyAllBc;
	timesToPrint[3] = ((0==_i00_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i00_stopWatch)/_i00_stopWatch.numMeasurements));
	timesToPrint[4] = getTotalTime(timer_CommunicateAll);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i01_stopWatch = timer_CommunicateAll;
	timesToPrint[5] = ((0==_i01_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i01_stopWatch)/_i01_stopWatch.numMeasurements));
	timesToPrint[6] = getTotalTime(timer_InitFieldsAnalytical);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i02_stopWatch = timer_InitFieldsAnalytical;
	timesToPrint[7] = ((0==_i02_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i02_stopWatch)/_i02_stopWatch.numMeasurements));
	timesToPrint[8] = getTotalTime(timer_MinDepth);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i03_stopWatch = timer_MinDepth;
	timesToPrint[9] = ((0==_i03_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i03_stopWatch)/_i03_stopWatch.numMeasurements));
	timesToPrint[10] = getTotalTime(timer_PrintDebugFields);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i04_stopWatch = timer_PrintDebugFields;
	timesToPrint[11] = ((0==_i04_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i04_stopWatch)/_i04_stopWatch.numMeasurements));
	timesToPrint[12] = getTotalTime(timer_SetupPhase);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i05_stopWatch = timer_SetupPhase;
	timesToPrint[13] = ((0==_i05_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i05_stopWatch)/_i05_stopWatch.numMeasurements));
	timesToPrint[14] = getTotalTime(timer_SolvePhase);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i06_stopWatch = timer_SolvePhase;
	timesToPrint[15] = ((0==_i06_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i06_stopWatch)/_i06_stopWatch.numMeasurements));
	timesToPrint[16] = getTotalTime(timer_UpdateCTilde);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i07_stopWatch = timer_UpdateCTilde;
	timesToPrint[17] = ((0==_i07_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i07_stopWatch)/_i07_stopWatch.numMeasurements));
	timesToPrint[18] = getTotalTime(timer_UpdateUnknowns);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i08_stopWatch = timer_UpdateUnknowns;
	timesToPrint[19] = ((0==_i08_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i08_stopWatch)/_i08_stopWatch.numMeasurements));
	timesToPrint[20] = getTotalTime(timer_print);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i09_stopWatch = timer_print;
	timesToPrint[21] = ((0==_i09_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i09_stopWatch)/_i09_stopWatch.numMeasurements));
	timesToPrint[22] = getTotalTime(timer_setup);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i10_stopWatch = timer_setup;
	timesToPrint[23] = ((0==_i10_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i10_stopWatch)/_i10_stopWatch.numMeasurements));
	timesToPrint[24] = getTotalTime(timer_timeLoop);
	/* -.-.-.- inlined getMeanTime -.-.-.- */
	StopWatch& _i11_stopWatch = timer_timeLoop;
	timesToPrint[25] = ((0==_i11_stopWatch.numMeasurements) ? 0.0 : (getTotalTime(_i11_stopWatch)/_i11_stopWatch.numMeasurements));
	for (int timerId = 0; timerId<26; ++timerId) {
		timesToPrint[timerId] /= 1;
	}
	std::ofstream outFile;
	outFile.open("timings.csv");
	outFile << "AdvanceRK2" << ";" << timesToPrint[0] << ";" << timesToPrint[1] << "\n";
	outFile << "ApplyAllBc" << ";" << timesToPrint[2] << ";" << timesToPrint[3] << "\n";
	outFile << "CommunicateAll" << ";" << timesToPrint[4] << ";" << timesToPrint[5] << "\n";
	outFile << "InitFieldsAnalytical" << ";" << timesToPrint[6] << ";" << timesToPrint[7] << "\n";
	outFile << "MinDepth" << ";" << timesToPrint[8] << ";" << timesToPrint[9] << "\n";
	outFile << "PrintDebugFields" << ";" << timesToPrint[10] << ";" << timesToPrint[11] << "\n";
	outFile << "SetupPhase" << ";" << timesToPrint[12] << ";" << timesToPrint[13] << "\n";
	outFile << "SolvePhase" << ";" << timesToPrint[14] << ";" << timesToPrint[15] << "\n";
	outFile << "UpdateCTilde" << ";" << timesToPrint[16] << ";" << timesToPrint[17] << "\n";
	outFile << "UpdateUnknowns" << ";" << timesToPrint[18] << ";" << timesToPrint[19] << "\n";
	outFile << "print" << ";" << timesToPrint[20] << ";" << timesToPrint[21] << "\n";
	outFile << "setup" << ";" << timesToPrint[22] << ";" << timesToPrint[23] << "\n";
	outFile << "timeLoop" << ";" << timesToPrint[24] << ";" << timesToPrint[25] << "\n";
	outFile.close();
}
