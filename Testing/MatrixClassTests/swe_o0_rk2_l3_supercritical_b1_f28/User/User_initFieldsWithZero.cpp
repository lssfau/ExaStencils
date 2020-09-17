#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void initFieldsWithZero () {
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_ETRI[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_UNRI[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_UTRI[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_adaptivity_nlockLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_adaptivity_nlockUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_bLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_bLower1[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_bLower2[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_bLower3[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_bUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_bUpper1[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_bUpper2[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_bUpper3[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_bath[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaEdgeDLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaEdgeDUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaEdgeHLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaEdgeHUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_cetaEdgeVLower0[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_cetaEdgeVUpper0[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaNewLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaNewUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaOldLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaOldUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuEdgeDLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuEdgeDUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuEdgeHLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuEdgeHUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_cuEdgeVLower0[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_cuEdgeVUpper0[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuNewLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuNewUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuOldLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuOldUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cuUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvEdgeDLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvEdgeDUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvEdgeHLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvEdgeHUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_cvEdgeVLower0[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_cvEdgeVUpper0[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvNewLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvNewUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvOldLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvOldUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_cvUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_detBInvLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_detBInvUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_detBLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_detBUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_local_orderLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_local_orderUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_normalVertical0[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_normalVertical1[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<10; i1 += 1) {
				for (int i0 = 0; i0<10; i0 += 1) {
					fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<11; i1 += 1) {
				for (int i0 = 0; i0<11; i0 += 1) {
					fieldData_tidal[fragmentIdx][((11*i1)+i0)] = 0.0;
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i3 = 0; i3<1; i3 += 1) {
				for (int i2 = 0; i2<2; i2 += 1) {
					for (int i1 = 0; i1<12; i1 += 1) {
						for (int i0 = 0; i0<12; i0 += 1) {
							fieldData_vf_cellCenter[fragmentIdx][((288*i3)+(144*i2)+(12*i1)+i0)] = 0.0;
						}
					}
				}
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i3 = 0; i3<1; i3 += 1) {
				for (int i2 = 0; i2<2; i2 += 1) {
					for (int i1 = 0; i1<13; i1 += 1) {
						for (int i0 = 0; i0<13; i0 += 1) {
							fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = 0.0;
						}
					}
				}
			}
		}
	}
}
