#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void InitTriInfo_3 () {
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_bLower0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]);
					fieldData_bLower1[fragmentIdx][((10*i1)+i0+11)] = (fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]);
					fieldData_bLower2[fragmentIdx][((10*i1)+i0+11)] = (fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]);
					fieldData_bLower3[fragmentIdx][((10*i1)+i0+11)] = (fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_detBLower0[fragmentIdx][((10*i1)+i0+11)] = std::fabs(((fieldData_bLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_bLower3[fragmentIdx][((10*i1)+i0+11)])-(fieldData_bLower1[fragmentIdx][((10*i1)+i0+11)]*fieldData_bLower2[fragmentIdx][((10*i1)+i0+11)])));
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_detBInvLower0[fragmentIdx][((10*i1)+i0+11)] = (1.0/fieldData_detBLower0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)] = std::sqrt(fieldData_detBLower0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)] = (1.0/fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_bUpper0[fragmentIdx][((10*i1)+i0+11)] = (fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)]);
					fieldData_bUpper1[fragmentIdx][((10*i1)+i0+11)] = (fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)]);
					fieldData_bUpper2[fragmentIdx][((10*i1)+i0+11)] = (fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)]);
					fieldData_bUpper3[fragmentIdx][((10*i1)+i0+11)] = (fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_detBUpper0[fragmentIdx][((10*i1)+i0+11)] = std::fabs(((fieldData_bUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_bUpper3[fragmentIdx][((10*i1)+i0+11)])-(fieldData_bUpper1[fragmentIdx][((10*i1)+i0+11)]*fieldData_bUpper2[fragmentIdx][((10*i1)+i0+11)])));
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_detBInvUpper0[fragmentIdx][((10*i1)+i0+11)] = (1.0/fieldData_detBUpper0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)] = std::sqrt(fieldData_detBUpper0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)] = (1.0/fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0+11)] = std::sqrt((((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)])*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]))+((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)])*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]))));
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = -1; i0<10; i0 += 1) {
					fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)] = std::sqrt((((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)])*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]))+((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)])*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]))));
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<10; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)] = std::sqrt((((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)])*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]))+((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)])*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]))));
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)])/fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)])/fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = -1; i0<10; i0 += 1) {
					fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)] = ((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)])/fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)] = ((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)])/fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<10; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)])/fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)] = ((fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)])/fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
	}
}
