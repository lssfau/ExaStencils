#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void SetupPhase_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_SetupPhase;
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i00_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	it = 0;
	t = 0.0;
	InitTriInfo_3();
	InitBath_3();
	CheckGrid_3();
	std::string fieldName_02;
	std::ostringstream string_builder_12;
	string_builder_12 << "./BSG/etri_b" << 0 << "_9.txt";
	fieldName_02 = string_builder_12.str();
	std::ifstream fieldReadStream_02(fieldName_02);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<9; i1 += 1) {
				for (int i0 = 0; i0<9; i0 += 1) {
					fieldReadStream_02 >> fieldData_ETRI[fragmentIdx][((11*i1)+i0+12)];
				}
			}
		}
	}
	fieldReadStream_02.close();
	std::string fieldName_03;
	std::ostringstream string_builder_13;
	string_builder_13 << "./BSG/unri_b" << 0 << "_9.txt";
	fieldName_03 = string_builder_13.str();
	std::ifstream fieldReadStream_03(fieldName_03);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<9; i1 += 1) {
				for (int i0 = 0; i0<9; i0 += 1) {
					fieldReadStream_03 >> fieldData_UNRI[fragmentIdx][((11*i1)+i0+12)];
				}
			}
		}
	}
	fieldReadStream_03.close();
	std::string fieldName_04;
	std::ostringstream string_builder_14;
	string_builder_14 << "./BSG/utri_b" << 0 << "_9.txt";
	fieldName_04 = string_builder_14.str();
	std::ifstream fieldReadStream_04(fieldName_04);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<9; i1 += 1) {
				for (int i0 = 0; i0<9; i0 += 1) {
					fieldReadStream_04 >> fieldData_UTRI[fragmentIdx][((11*i1)+i0+12)];
				}
			}
		}
	}
	fieldReadStream_04.close();
	InitFieldsAnalytical_3();
	UpdateCTilde_3();
	CommunicateAll_3();
	ApplyAllBc_3();
	PrintDebugFields_3();
	print_count += 1;
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_SetupPhase;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
