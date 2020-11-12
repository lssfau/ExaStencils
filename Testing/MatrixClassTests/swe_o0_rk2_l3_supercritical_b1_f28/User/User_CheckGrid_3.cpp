#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void CheckGrid_3 () {
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					if ((fieldData_detBLower0[fragmentIdx][((10*i1)+i0+11)]<=0)) {
						std::cout << "Warning:, detBLower0,  at " << " " << "fragmentIdx =" << " " << fragmentIdx << " " << "i0 =" << " " << i0 << " " << "i1 =" << " " << i1 << " " << "is" << " " << fieldData_detBLower0[fragmentIdx][((10*i1)+i0+11)] << " " << std::endl;
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					if ((fieldData_detBLower0[fragmentIdx][((10*i1)+i0+11)]<=0)) {
						std::cout << "Warning:, detBLower0,  at " << " " << "fragmentIdx =" << " " << fragmentIdx << " " << "i0 =" << " " << i0 << " " << "i1 =" << " " << i1 << " " << "is" << " " << fieldData_detBLower0[fragmentIdx][((10*i1)+i0+11)] << " " << std::endl;
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					if ((fieldData_detBUpper0[fragmentIdx][((10*i1)+i0+11)]<=0)) {
						std::cout << "Warning:, detBUpper0,  at " << " " << "fragmentIdx =" << " " << fragmentIdx << " " << "i0 =" << " " << i0 << " " << "i1 =" << " " << i1 << " " << "is" << " " << fieldData_detBUpper0[fragmentIdx][((10*i1)+i0+11)] << " " << std::endl;
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					if ((fieldData_detBUpper0[fragmentIdx][((10*i1)+i0+11)]<=0)) {
						std::cout << "Warning:, detBUpper0,  at " << " " << "fragmentIdx =" << " " << fragmentIdx << " " << "i0 =" << " " << i0 << " " << "i1 =" << " " << i1 << " " << "is" << " " << fieldData_detBUpper0[fragmentIdx][((10*i1)+i0+11)] << " " << std::endl;
					}
				}
			}
		}
	}
}
