#include "fstream"
#include "sstream"
#include "iomanip"

#include "Communication/Communication.h"

void exchcvLower0_all_a_a_a_a_3 (int slot) {
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (neighbor_isValid[0][fragmentIdx]) {
			
		}
		if (neighbor_isValid[1][fragmentIdx]) {
			
		}
		if (neighbor_isValid[2][fragmentIdx]) {
			
		}
		if (neighbor_isValid[3][fragmentIdx]) {
			
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (neighbor_isValid[0][fragmentIdx]) {
				;
				switch (commTrafoId[0][fragmentIdx]) {
					case 0: {
						for (int i1 = 1; i1<9; i1 += 1) {
							for (int i0 = 0; i0<1; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvLower0[neighbor_fragCommId[0][fragmentIdx]][((10*i1)+i0+8)];
							}
						}
					} break;
					case 1: {
						for (int i1 = 1; i1<9; i1 += 1) {
							for (int i0 = 0; i0<1; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvLower0[neighbor_fragCommId[0][fragmentIdx]][(((10*i0)+89)-i1)];
							}
						}
					} break;
					case 2: {
						for (int i1 = 1; i1<9; i1 += 1) {
							for (int i0 = 0; i0<1; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvUpper0[neighbor_fragCommId[0][fragmentIdx]][(91-((10*i1)+i0))];
							}
						}
					} break;
					case 3: {
						for (int i1 = 1; i1<9; i1 += 1) {
							for (int i0 = 0; i0<1; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvUpper0[neighbor_fragCommId[0][fragmentIdx]][((i1+10)-(10*i0))];
							}
						}
					} break;
				}
				
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				;
				switch (commTrafoId[1][fragmentIdx]) {
					case 0: {
						for (int i1 = 1; i1<9; i1 += 1) {
							for (int i0 = 9; i0<10; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvLower0[neighbor_fragCommId[1][fragmentIdx]][(((10*i1)+i0)-8)];
							}
						}
					} break;
					case 1: {
						for (int i1 = 1; i1<9; i1 += 1) {
							for (int i0 = 9; i0<10; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvLower0[neighbor_fragCommId[1][fragmentIdx]][((10*i0)-(i1+71))];
							}
						}
					} break;
					case 2: {
						for (int i1 = 1; i1<9; i1 += 1) {
							for (int i0 = 9; i0<10; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvUpper0[neighbor_fragCommId[1][fragmentIdx]][(107-((10*i1)+i0))];
							}
						}
					} break;
					case 3: {
						for (int i1 = 1; i1<9; i1 += 1) {
							for (int i0 = 9; i0<10; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvUpper0[neighbor_fragCommId[1][fragmentIdx]][((i1+170)-(10*i0))];
							}
						}
					} break;
				}
				
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				;
				switch (commTrafoId[2][fragmentIdx]) {
					case 0: {
						for (int i1 = 0; i1<1; i1 += 1) {
							for (int i0 = 1; i0<9; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvLower0[neighbor_fragCommId[2][fragmentIdx]][((10*i1)+i0+80)];
							}
						}
					} break;
					case 1: {
						for (int i1 = 0; i1<1; i1 += 1) {
							for (int i0 = 1; i0<9; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvUpper0[neighbor_fragCommId[2][fragmentIdx]][(((10*i0)+1)-i1)];
							}
						}
					} break;
					case 2: {
						for (int i1 = 0; i1<1; i1 += 1) {
							for (int i0 = 1; i0<9; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvUpper0[neighbor_fragCommId[2][fragmentIdx]][(19-((10*i1)+i0))];
							}
						}
					} break;
					case 3: {
						for (int i1 = 0; i1<1; i1 += 1) {
							for (int i0 = 1; i0<9; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvLower0[neighbor_fragCommId[2][fragmentIdx]][((i1+98)-(10*i0))];
							}
						}
					} break;
				}
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				;
				switch (commTrafoId[3][fragmentIdx]) {
					case 0: {
						for (int i1 = 9; i1<10; i1 += 1) {
							for (int i0 = 1; i0<9; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvLower0[neighbor_fragCommId[3][fragmentIdx]][(((10*i1)+i0)-80)];
							}
						}
					} break;
					case 1: {
						for (int i1 = 9; i1<10; i1 += 1) {
							for (int i0 = 1; i0<9; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvUpper0[neighbor_fragCommId[3][fragmentIdx]][(((10*i0)+17)-i1)];
							}
						}
					} break;
					case 2: {
						for (int i1 = 9; i1<10; i1 += 1) {
							for (int i0 = 1; i0<9; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvUpper0[neighbor_fragCommId[3][fragmentIdx]][(179-((10*i1)+i0))];
							}
						}
					} break;
					case 3: {
						for (int i1 = 9; i1<10; i1 += 1) {
							for (int i0 = 1; i0<9; i0 += 1) {
								fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = fieldData_cvLower0[neighbor_fragCommId[3][fragmentIdx]][((i1+82)-(10*i0))];
							}
						}
					} break;
				}
				
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (neighbor_isValid[0][fragmentIdx]) {
			;
		}
		if (neighbor_isValid[1][fragmentIdx]) {
			;
		}
		if (neighbor_isValid[2][fragmentIdx]) {
			;
		}
		if (neighbor_isValid[3][fragmentIdx]) {
			;
		}
	}
}
