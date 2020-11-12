#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void UpdateCTilde_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_UpdateCTilde;
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i00_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					{
						double _local_unknowns = 0.0;
						double _local_rhs = 0.0;
						double _local_matrix_0_0;
						if ((((((!(!(neighbor_isValid[0][fragmentIdx]))||(i0>=0))&&(!(!(neighbor_isValid[1][fragmentIdx]))||(i0<8)))&&(!(!(neighbor_isValid[2][fragmentIdx]))||(i1>=0)))&&(!(!(neighbor_isValid[3][fragmentIdx]))||(i1<8)))&&(((i0<8)&&(i0>=0))&&((i1<8)&&(i1>=0))))) {
							_local_rhs = fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)];
							_local_matrix_0_0 = ((1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.333333333333333*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])));
						} else {
							_local_rhs = fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)];
							_local_matrix_0_0 = 1.0;
						}
						_local_unknowns = ((1.0/_local_matrix_0_0)*_local_rhs);
						if ((((((!(!(neighbor_isValid[0][fragmentIdx]))||(i0>=0))&&(!(!(neighbor_isValid[1][fragmentIdx]))||(i0<8)))&&(!(!(neighbor_isValid[2][fragmentIdx]))||(i1>=0)))&&(!(!(neighbor_isValid[3][fragmentIdx]))||(i1<8)))&&(((i0<8)&&(i0>=0))&&((i1<8)&&(i1>=0))))) {
							fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)] = _local_unknowns;
						}
					}
					{
						double _local_unknowns = 0.0;
						double _local_rhs = 0.0;
						double _local_matrix_0_0;
						if ((((((!(!(neighbor_isValid[0][fragmentIdx]))||(i0>=0))&&(!(!(neighbor_isValid[1][fragmentIdx]))||(i0<8)))&&(!(!(neighbor_isValid[2][fragmentIdx]))||(i1>=0)))&&(!(!(neighbor_isValid[3][fragmentIdx]))||(i1<8)))&&(((i0<8)&&(i0>=0))&&((i1<8)&&(i1>=0))))) {
							_local_rhs = fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)];
							_local_matrix_0_0 = ((1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.333333333333333*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)])));
						} else {
							_local_rhs = fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+11)];
							_local_matrix_0_0 = 1.0;
						}
						_local_unknowns = ((1.0/_local_matrix_0_0)*_local_rhs);
						if ((((((!(!(neighbor_isValid[0][fragmentIdx]))||(i0>=0))&&(!(!(neighbor_isValid[1][fragmentIdx]))||(i0<8)))&&(!(!(neighbor_isValid[2][fragmentIdx]))||(i1>=0)))&&(!(!(neighbor_isValid[3][fragmentIdx]))||(i1<8)))&&(((i0<8)&&(i0>=0))&&((i1<8)&&(i1>=0))))) {
							fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+11)] = _local_unknowns;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					{
						double _local_unknowns = 0.0;
						double _local_rhs = 0.0;
						double _local_matrix_0_0;
						if ((((((!(!(neighbor_isValid[0][fragmentIdx]))||(i0>=0))&&(!(!(neighbor_isValid[1][fragmentIdx]))||(i0<8)))&&(!(!(neighbor_isValid[2][fragmentIdx]))||(i1>=0)))&&(!(!(neighbor_isValid[3][fragmentIdx]))||(i1<8)))&&(((i0<8)&&(i0>=0))&&((i1<8)&&(i1>=0))))) {
							_local_rhs = fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)];
							_local_matrix_0_0 = ((1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.333333333333333*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])));
						} else {
							_local_rhs = fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)];
							_local_matrix_0_0 = 1.0;
						}
						_local_unknowns = ((1.0/_local_matrix_0_0)*_local_rhs);
						if ((((((!(!(neighbor_isValid[0][fragmentIdx]))||(i0>=0))&&(!(!(neighbor_isValid[1][fragmentIdx]))||(i0<8)))&&(!(!(neighbor_isValid[2][fragmentIdx]))||(i1>=0)))&&(!(!(neighbor_isValid[3][fragmentIdx]))||(i1<8)))&&(((i0<8)&&(i0>=0))&&((i1<8)&&(i1>=0))))) {
							fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)] = _local_unknowns;
						}
					}
					{
						double _local_unknowns = 0.0;
						double _local_rhs = 0.0;
						double _local_matrix_0_0;
						if ((((((!(!(neighbor_isValid[0][fragmentIdx]))||(i0>=0))&&(!(!(neighbor_isValid[1][fragmentIdx]))||(i0<8)))&&(!(!(neighbor_isValid[2][fragmentIdx]))||(i1>=0)))&&(!(!(neighbor_isValid[3][fragmentIdx]))||(i1<8)))&&(((i0<8)&&(i0>=0))&&((i1<8)&&(i1>=0))))) {
							_local_rhs = fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)];
							_local_matrix_0_0 = ((1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.333333333333333*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)])));
						} else {
							_local_rhs = fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+11)];
							_local_matrix_0_0 = 1.0;
						}
						_local_unknowns = ((1.0/_local_matrix_0_0)*_local_rhs);
						if ((((((!(!(neighbor_isValid[0][fragmentIdx]))||(i0>=0))&&(!(!(neighbor_isValid[1][fragmentIdx]))||(i0<8)))&&(!(!(neighbor_isValid[2][fragmentIdx]))||(i1>=0)))&&(!(!(neighbor_isValid[3][fragmentIdx]))||(i1<8)))&&(((i0<8)&&(i0>=0))&&((i1<8)&&(i1>=0))))) {
							fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+11)] = _local_unknowns;
						}
					}
				}
			}
		}
	}
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_UpdateCTilde;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
