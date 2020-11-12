#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void ApplyAllBc_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_ApplyAllBc;
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i00_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[0][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = -1; i0<0; i0 += 1) {
						if ((-1300==boundaryConditionId[0][fragmentIdx])) {
							fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)]);
						} else if ((-1200==boundaryConditionId[0][fragmentIdx])) {
							fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.353553390593275*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+13)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[0][fragmentIdx])) {
							fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
						} else if (((-100==boundaryConditionId[0][fragmentIdx])||(-101==boundaryConditionId[0][fragmentIdx]))) {
							fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[0][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[0][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = -1; i0<0; i0 += 1) {
						if ((-1300==boundaryConditionId[0][fragmentIdx])) {
							fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cuLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)]);
						} else if ((-1200==boundaryConditionId[0][fragmentIdx])) {
							fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.1767766952966375*(((fieldData_UNRI[fragmentIdx][((11*i1)+i0+13)]+fieldData_UNRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+13)])-((fieldData_UTRI[fragmentIdx][((11*i1)+i0+13)]+fieldData_UTRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+13)]))*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+13)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+24)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)])*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[0][fragmentIdx])) {
							fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cuLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)]);
						} else if (((-100==boundaryConditionId[0][fragmentIdx])||(-101==boundaryConditionId[0][fragmentIdx]))) {
							fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*((1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)])-(2.8284271247462*((fieldData_cuLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+13)]*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+13)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)])+(fieldData_cvLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+13)]*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+13)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)]))))*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[0][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[0][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = -1; i0<0; i0 += 1) {
						if ((-1300==boundaryConditionId[0][fragmentIdx])) {
							fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cvLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)]);
						} else if ((-1200==boundaryConditionId[0][fragmentIdx])) {
							fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.1767766952966375*(((fieldData_UNRI[fragmentIdx][((11*i1)+i0+13)]+fieldData_UNRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+13)])+((fieldData_UTRI[fragmentIdx][((11*i1)+i0+13)]+fieldData_UTRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+13)]))*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+13)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+24)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)])*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[0][fragmentIdx])) {
							fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cvLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)]);
						} else if (((-100==boundaryConditionId[0][fragmentIdx])||(-101==boundaryConditionId[0][fragmentIdx]))) {
							fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*((1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)])-(2.8284271247462*((fieldData_cuLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+13)]*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+13)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)])+(fieldData_cvLower0[fragmentIdx][((10*i1)+i0+12)]*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+13)]*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+13)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+12)]))))*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[0][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[1][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = 8; i0<9; i0 += 1) {
						if ((-1300==boundaryConditionId[1][fragmentIdx])) {
							fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						} else if ((-1200==boundaryConditionId[1][fragmentIdx])) {
							fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = (0.353553390593275*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+23)])*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[1][fragmentIdx])) {
							fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
						} else if (((-100==boundaryConditionId[1][fragmentIdx])||(-101==boundaryConditionId[1][fragmentIdx]))) {
							fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[1][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[1][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = 8; i0<9; i0 += 1) {
						if ((-1300==boundaryConditionId[1][fragmentIdx])) {
							fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						} else if ((-1200==boundaryConditionId[1][fragmentIdx])) {
							fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (0.1767766952966375*(((fieldData_UTRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_UTRI[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])-((fieldData_UNRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_UNRI[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]))*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[1][fragmentIdx])) {
							fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						} else if (((-100==boundaryConditionId[1][fragmentIdx])||(-101==boundaryConditionId[1][fragmentIdx]))) {
							fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*((1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)])-(2.8284271247462*((fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)])+(fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]))))*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[1][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[1][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = 8; i0<9; i0 += 1) {
						if ((-1300==boundaryConditionId[1][fragmentIdx])) {
							fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						} else if ((-1200==boundaryConditionId[1][fragmentIdx])) {
							fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = (-(0.1767766952966375*(((fieldData_UNRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_UNRI[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+((fieldData_UTRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_UTRI[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]))*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]));
						} else if ((-1100==boundaryConditionId[1][fragmentIdx])) {
							fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						} else if (((-100==boundaryConditionId[1][fragmentIdx])||(-101==boundaryConditionId[1][fragmentIdx]))) {
							fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*((1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)])-(2.8284271247462*((fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)])+(fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]))))*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[1][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[2][fragmentIdx])) {
				for (int i1 = -1; i1<0; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						if ((-1300==boundaryConditionId[2][fragmentIdx])) {
							fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)]);
						} else if ((-1200==boundaryConditionId[2][fragmentIdx])) {
							fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.353553390593275*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+23)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[2][fragmentIdx])) {
							fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
						} else if (((-100==boundaryConditionId[2][fragmentIdx])||(-101==boundaryConditionId[2][fragmentIdx]))) {
							fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[2][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[2][fragmentIdx])) {
				for (int i1 = -1; i1<0; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						if ((-1300==boundaryConditionId[2][fragmentIdx])) {
							fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cuLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)]);
						} else if ((-1200==boundaryConditionId[2][fragmentIdx])) {
							fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.1767766952966375*(((fieldData_UNRI[fragmentIdx][((11*i1)+i0+23)]+fieldData_UNRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+21)])-((fieldData_UTRI[fragmentIdx][((11*i1)+i0+23)]+fieldData_UTRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+21)]))*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+23)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+24)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)])*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[2][fragmentIdx])) {
							fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cuLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)]);
						} else if (((-100==boundaryConditionId[2][fragmentIdx])||(-101==boundaryConditionId[2][fragmentIdx]))) {
							fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*((1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)])-(2.8284271247462*((fieldData_cuLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+21)]*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)])+(fieldData_cvLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+21)]*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)]))))*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[2][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[2][fragmentIdx])) {
				for (int i1 = -1; i1<0; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						if ((-1300==boundaryConditionId[2][fragmentIdx])) {
							fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cvLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)]);
						} else if ((-1200==boundaryConditionId[2][fragmentIdx])) {
							fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.1767766952966375*(((fieldData_UNRI[fragmentIdx][((11*i1)+i0+23)]+fieldData_UNRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+21)])+((fieldData_UTRI[fragmentIdx][((11*i1)+i0+23)]+fieldData_UTRI[fragmentIdx][((11*i1)+i0+24)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+21)]))*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+23)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+24)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)])*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[2][fragmentIdx])) {
							fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cvLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)]);
						} else if (((-100==boundaryConditionId[2][fragmentIdx])||(-101==boundaryConditionId[2][fragmentIdx]))) {
							fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*((1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)])-(2.8284271247462*((fieldData_cuLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+21)]*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)])+(fieldData_cvLower0[fragmentIdx][((10*i1)+i0+21)]*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+21)]*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+21)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+21)]))))*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[2][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[3][fragmentIdx])) {
				for (int i1 = 8; i1<9; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						if ((-1300==boundaryConditionId[3][fragmentIdx])) {
							fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						} else if ((-1200==boundaryConditionId[3][fragmentIdx])) {
							fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = (0.353553390593275*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+13)])*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[3][fragmentIdx])) {
							fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = 0.0;
						} else if (((-100==boundaryConditionId[3][fragmentIdx])||(-101==boundaryConditionId[3][fragmentIdx]))) {
							fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[3][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[3][fragmentIdx])) {
				for (int i1 = 8; i1<9; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						if ((-1300==boundaryConditionId[3][fragmentIdx])) {
							fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						} else if ((-1200==boundaryConditionId[3][fragmentIdx])) {
							fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (0.1767766952966375*(((fieldData_UTRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_UTRI[fragmentIdx][((11*i1)+i0+13)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])-((fieldData_UNRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_UNRI[fragmentIdx][((11*i1)+i0+13)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]))*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)])*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						} else if ((-1100==boundaryConditionId[3][fragmentIdx])) {
							fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						} else if (((-100==boundaryConditionId[3][fragmentIdx])||(-101==boundaryConditionId[3][fragmentIdx]))) {
							fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*((1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)])-(2.8284271247462*((fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)])+(fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]))))*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[3][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[3][fragmentIdx])) {
				for (int i1 = 8; i1<9; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						if ((-1300==boundaryConditionId[3][fragmentIdx])) {
							fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						} else if ((-1200==boundaryConditionId[3][fragmentIdx])) {
							fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = (-(0.1767766952966375*(((fieldData_UNRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_UNRI[fragmentIdx][((11*i1)+i0+13)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+((fieldData_UTRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_UTRI[fragmentIdx][((11*i1)+i0+13)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]))*(fieldData_ETRI[fragmentIdx][((11*i1)+i0+12)]+fieldData_ETRI[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)])*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]));
						} else if ((-1100==boundaryConditionId[3][fragmentIdx])) {
							fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						} else if (((-100==boundaryConditionId[3][fragmentIdx])||(-101==boundaryConditionId[3][fragmentIdx]))) {
							fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*((1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)])-(2.8284271247462*((fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)])+(fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]))))*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						} else {
							std::cout << "Invalid boundary condition id" << " " << boundaryConditionId[3][fragmentIdx] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[0][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = -1; i0<0; i0 += 1) {
						fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])/((1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)]))));
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[0][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = -1; i0<0; i0 += 1) {
						fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])/((1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)]))));
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[1][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = 8; i0<9; i0 += 1) {
						fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])/((1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]))));
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[1][fragmentIdx])) {
				for (int i1 = -1; i1<9; i1 += 1) {
					for (int i0 = 8; i0<9; i0 += 1) {
						fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])/((1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]))));
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[2][fragmentIdx])) {
				for (int i1 = -1; i1<0; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])/((1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)]))));
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[2][fragmentIdx])) {
				for (int i1 = -1; i1<0; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])/((1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)]))));
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[3][fragmentIdx])) {
				for (int i1 = 8; i1<9; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])/((1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]))));
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[3][fragmentIdx])) {
				for (int i1 = 8; i1<9; i1 += 1) {
					for (int i0 = -1; i0<9; i0 += 1) {
						fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])/((1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]))));
					}
				}
			}
		}
	}
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_ApplyAllBc;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
