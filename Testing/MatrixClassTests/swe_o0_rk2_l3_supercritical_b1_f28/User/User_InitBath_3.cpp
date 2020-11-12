#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void InitBath_3 () {
	std::string fieldName_01;
	std::ostringstream string_builder_01;
	string_builder_01 << "./BSG/bath_b" << 0 << "_9.txt";
	fieldName_01 = string_builder_01.str();
	std::ifstream fieldReadStream_01(fieldName_01);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<9; i1 += 1) {
				for (int i0 = 0; i0<9; i0 += 1) {
					fieldReadStream_01 >> fieldData_bath[fragmentIdx][((11*i1)+i0+12)];
				}
			}
		}
	}
	fieldReadStream_01.close();
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (!(neighbor_isValid[0][fragmentIdx])) {
				for (int i1 = -1; i1<10; i1 += 1) {
					for (int i0 = -1; i0<0; i0 += 1) {
						fieldData_bath[fragmentIdx][((11*i1)+i0+12)] = fieldData_bath[fragmentIdx][((11*i1)+i0+13)];
						if ((fieldData_bath[fragmentIdx][((11*i1)+i0+12)]<0)) {
							std::cout << "Negative bathymetry detected:" << " " << fieldData_bath[fragmentIdx][((11*i1)+i0+12)] << " " << std::endl;
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
				for (int i1 = -1; i1<10; i1 += 1) {
					for (int i0 = 9; i0<10; i0 += 1) {
						fieldData_bath[fragmentIdx][((11*i1)+i0+12)] = fieldData_bath[fragmentIdx][((11*i1)+i0+11)];
						if ((fieldData_bath[fragmentIdx][((11*i1)+i0+12)]<0)) {
							std::cout << "Negative bathymetry detected:" << " " << fieldData_bath[fragmentIdx][((11*i1)+i0+12)] << " " << std::endl;
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
					for (int i0 = -1; i0<10; i0 += 1) {
						fieldData_bath[fragmentIdx][((11*i1)+i0+12)] = fieldData_bath[fragmentIdx][((11*i1)+i0+23)];
						if ((fieldData_bath[fragmentIdx][((11*i1)+i0+12)]<0)) {
							std::cout << "Negative bathymetry detected:" << " " << fieldData_bath[fragmentIdx][((11*i1)+i0+12)] << " " << std::endl;
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
				for (int i1 = 9; i1<10; i1 += 1) {
					for (int i0 = -1; i0<10; i0 += 1) {
						fieldData_bath[fragmentIdx][((11*i1)+i0+12)] = fieldData_bath[fragmentIdx][((11*i1)+i0+1)];
						if ((fieldData_bath[fragmentIdx][((11*i1)+i0+12)]<0)) {
							std::cout << "Negative bathymetry detected:" << " " << fieldData_bath[fragmentIdx][((11*i1)+i0+12)] << " " << std::endl;
						}
					}
				}
			}
		}
	}
	exchbath_all_a_a_a_a_3(0);
}
