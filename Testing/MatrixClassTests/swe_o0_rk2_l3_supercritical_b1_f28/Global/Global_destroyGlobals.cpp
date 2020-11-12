#include "fstream"
#include "sstream"
#include "iomanip"

#include "Global/Global.h"

void destroyGlobals () {
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_ETRI[fragmentIdx]) {
			delete[] fieldData_ETRI[fragmentIdx];
			fieldData_ETRI[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_UNRI[fragmentIdx]) {
			delete[] fieldData_UNRI[fragmentIdx];
			fieldData_UNRI[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_UTRI[fragmentIdx]) {
			delete[] fieldData_UTRI[fragmentIdx];
			fieldData_UTRI[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_adaptivity_nlockLower0[fragmentIdx]) {
			delete[] fieldData_adaptivity_nlockLower0[fragmentIdx];
			fieldData_adaptivity_nlockLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_adaptivity_nlockUpper0[fragmentIdx]) {
			delete[] fieldData_adaptivity_nlockUpper0[fragmentIdx];
			fieldData_adaptivity_nlockUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_bLower0[fragmentIdx]) {
			delete[] fieldData_bLower0[fragmentIdx];
			fieldData_bLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_bLower1[fragmentIdx]) {
			delete[] fieldData_bLower1[fragmentIdx];
			fieldData_bLower1[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_bLower2[fragmentIdx]) {
			delete[] fieldData_bLower2[fragmentIdx];
			fieldData_bLower2[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_bLower3[fragmentIdx]) {
			delete[] fieldData_bLower3[fragmentIdx];
			fieldData_bLower3[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_bUpper0[fragmentIdx]) {
			delete[] fieldData_bUpper0[fragmentIdx];
			fieldData_bUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_bUpper1[fragmentIdx]) {
			delete[] fieldData_bUpper1[fragmentIdx];
			fieldData_bUpper1[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_bUpper2[fragmentIdx]) {
			delete[] fieldData_bUpper2[fragmentIdx];
			fieldData_bUpper2[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_bUpper3[fragmentIdx]) {
			delete[] fieldData_bUpper3[fragmentIdx];
			fieldData_bUpper3[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_bath[fragmentIdx]) {
			delete[] fieldData_bath[fragmentIdx];
			fieldData_bath[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaEdgeDLower0[fragmentIdx]) {
			delete[] fieldData_cetaEdgeDLower0[fragmentIdx];
			fieldData_cetaEdgeDLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaEdgeDUpper0[fragmentIdx]) {
			delete[] fieldData_cetaEdgeDUpper0[fragmentIdx];
			fieldData_cetaEdgeDUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaEdgeHLower0[fragmentIdx]) {
			delete[] fieldData_cetaEdgeHLower0[fragmentIdx];
			fieldData_cetaEdgeHLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaEdgeHUpper0[fragmentIdx]) {
			delete[] fieldData_cetaEdgeHUpper0[fragmentIdx];
			fieldData_cetaEdgeHUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaEdgeVLower0[fragmentIdx]) {
			delete[] fieldData_cetaEdgeVLower0[fragmentIdx];
			fieldData_cetaEdgeVLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaEdgeVUpper0[fragmentIdx]) {
			delete[] fieldData_cetaEdgeVUpper0[fragmentIdx];
			fieldData_cetaEdgeVUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaLower0[fragmentIdx]) {
			delete[] fieldData_cetaLower0[fragmentIdx];
			fieldData_cetaLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaNewLower0[fragmentIdx]) {
			delete[] fieldData_cetaNewLower0[fragmentIdx];
			fieldData_cetaNewLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaNewUpper0[fragmentIdx]) {
			delete[] fieldData_cetaNewUpper0[fragmentIdx];
			fieldData_cetaNewUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaOldLower0[fragmentIdx]) {
			delete[] fieldData_cetaOldLower0[fragmentIdx];
			fieldData_cetaOldLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaOldUpper0[fragmentIdx]) {
			delete[] fieldData_cetaOldUpper0[fragmentIdx];
			fieldData_cetaOldUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cetaUpper0[fragmentIdx]) {
			delete[] fieldData_cetaUpper0[fragmentIdx];
			fieldData_cetaUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuEdgeDLower0[fragmentIdx]) {
			delete[] fieldData_cuEdgeDLower0[fragmentIdx];
			fieldData_cuEdgeDLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuEdgeDUpper0[fragmentIdx]) {
			delete[] fieldData_cuEdgeDUpper0[fragmentIdx];
			fieldData_cuEdgeDUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuEdgeHLower0[fragmentIdx]) {
			delete[] fieldData_cuEdgeHLower0[fragmentIdx];
			fieldData_cuEdgeHLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuEdgeHUpper0[fragmentIdx]) {
			delete[] fieldData_cuEdgeHUpper0[fragmentIdx];
			fieldData_cuEdgeHUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuEdgeVLower0[fragmentIdx]) {
			delete[] fieldData_cuEdgeVLower0[fragmentIdx];
			fieldData_cuEdgeVLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuEdgeVUpper0[fragmentIdx]) {
			delete[] fieldData_cuEdgeVUpper0[fragmentIdx];
			fieldData_cuEdgeVUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuLower0[fragmentIdx]) {
			delete[] fieldData_cuLower0[fragmentIdx];
			fieldData_cuLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuNewLower0[fragmentIdx]) {
			delete[] fieldData_cuNewLower0[fragmentIdx];
			fieldData_cuNewLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuNewUpper0[fragmentIdx]) {
			delete[] fieldData_cuNewUpper0[fragmentIdx];
			fieldData_cuNewUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuOldLower0[fragmentIdx]) {
			delete[] fieldData_cuOldLower0[fragmentIdx];
			fieldData_cuOldLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuOldUpper0[fragmentIdx]) {
			delete[] fieldData_cuOldUpper0[fragmentIdx];
			fieldData_cuOldUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuTildeLower0[fragmentIdx]) {
			delete[] fieldData_cuTildeLower0[fragmentIdx];
			fieldData_cuTildeLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuTildeUpper0[fragmentIdx]) {
			delete[] fieldData_cuTildeUpper0[fragmentIdx];
			fieldData_cuTildeUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cuUpper0[fragmentIdx]) {
			delete[] fieldData_cuUpper0[fragmentIdx];
			fieldData_cuUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvEdgeDLower0[fragmentIdx]) {
			delete[] fieldData_cvEdgeDLower0[fragmentIdx];
			fieldData_cvEdgeDLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvEdgeDUpper0[fragmentIdx]) {
			delete[] fieldData_cvEdgeDUpper0[fragmentIdx];
			fieldData_cvEdgeDUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvEdgeHLower0[fragmentIdx]) {
			delete[] fieldData_cvEdgeHLower0[fragmentIdx];
			fieldData_cvEdgeHLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvEdgeHUpper0[fragmentIdx]) {
			delete[] fieldData_cvEdgeHUpper0[fragmentIdx];
			fieldData_cvEdgeHUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvEdgeVLower0[fragmentIdx]) {
			delete[] fieldData_cvEdgeVLower0[fragmentIdx];
			fieldData_cvEdgeVLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvEdgeVUpper0[fragmentIdx]) {
			delete[] fieldData_cvEdgeVUpper0[fragmentIdx];
			fieldData_cvEdgeVUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvLower0[fragmentIdx]) {
			delete[] fieldData_cvLower0[fragmentIdx];
			fieldData_cvLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvNewLower0[fragmentIdx]) {
			delete[] fieldData_cvNewLower0[fragmentIdx];
			fieldData_cvNewLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvNewUpper0[fragmentIdx]) {
			delete[] fieldData_cvNewUpper0[fragmentIdx];
			fieldData_cvNewUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvOldLower0[fragmentIdx]) {
			delete[] fieldData_cvOldLower0[fragmentIdx];
			fieldData_cvOldLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvOldUpper0[fragmentIdx]) {
			delete[] fieldData_cvOldUpper0[fragmentIdx];
			fieldData_cvOldUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvTildeLower0[fragmentIdx]) {
			delete[] fieldData_cvTildeLower0[fragmentIdx];
			fieldData_cvTildeLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvTildeUpper0[fragmentIdx]) {
			delete[] fieldData_cvTildeUpper0[fragmentIdx];
			fieldData_cvTildeUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_cvUpper0[fragmentIdx]) {
			delete[] fieldData_cvUpper0[fragmentIdx];
			fieldData_cvUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_detBInvLower0[fragmentIdx]) {
			delete[] fieldData_detBInvLower0[fragmentIdx];
			fieldData_detBInvLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_detBInvUpper0[fragmentIdx]) {
			delete[] fieldData_detBInvUpper0[fragmentIdx];
			fieldData_detBInvUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_detBLower0[fragmentIdx]) {
			delete[] fieldData_detBLower0[fragmentIdx];
			fieldData_detBLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_detBUpper0[fragmentIdx]) {
			delete[] fieldData_detBUpper0[fragmentIdx];
			fieldData_detBUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_edgeLenDiagonal0[fragmentIdx]) {
			delete[] fieldData_edgeLenDiagonal0[fragmentIdx];
			fieldData_edgeLenDiagonal0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_edgeLenHorizontal0[fragmentIdx]) {
			delete[] fieldData_edgeLenHorizontal0[fragmentIdx];
			fieldData_edgeLenHorizontal0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_edgeLenVertical0[fragmentIdx]) {
			delete[] fieldData_edgeLenVertical0[fragmentIdx];
			fieldData_edgeLenVertical0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_local_orderLower0[fragmentIdx]) {
			delete[] fieldData_local_orderLower0[fragmentIdx];
			fieldData_local_orderLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_local_orderUpper0[fragmentIdx]) {
			delete[] fieldData_local_orderUpper0[fragmentIdx];
			fieldData_local_orderUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_normalDiagonal0[fragmentIdx]) {
			delete[] fieldData_normalDiagonal0[fragmentIdx];
			fieldData_normalDiagonal0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_normalDiagonal1[fragmentIdx]) {
			delete[] fieldData_normalDiagonal1[fragmentIdx];
			fieldData_normalDiagonal1[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_normalHorizontal0[fragmentIdx]) {
			delete[] fieldData_normalHorizontal0[fragmentIdx];
			fieldData_normalHorizontal0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_normalHorizontal1[fragmentIdx]) {
			delete[] fieldData_normalHorizontal1[fragmentIdx];
			fieldData_normalHorizontal1[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_normalVertical0[fragmentIdx]) {
			delete[] fieldData_normalVertical0[fragmentIdx];
			fieldData_normalVertical0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_normalVertical1[fragmentIdx]) {
			delete[] fieldData_normalVertical1[fragmentIdx];
			fieldData_normalVertical1[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_sqrtDetBLower0[fragmentIdx]) {
			delete[] fieldData_sqrtDetBLower0[fragmentIdx];
			fieldData_sqrtDetBLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_sqrtDetBUpper0[fragmentIdx]) {
			delete[] fieldData_sqrtDetBUpper0[fragmentIdx];
			fieldData_sqrtDetBUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_sqrtInvDetBLower0[fragmentIdx]) {
			delete[] fieldData_sqrtInvDetBLower0[fragmentIdx];
			fieldData_sqrtInvDetBLower0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_sqrtInvDetBUpper0[fragmentIdx]) {
			delete[] fieldData_sqrtInvDetBUpper0[fragmentIdx];
			fieldData_sqrtInvDetBUpper0[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_tidal[fragmentIdx]) {
			delete[] fieldData_tidal[fragmentIdx];
			fieldData_tidal[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_vf_cellCenter[fragmentIdx]) {
			delete[] fieldData_vf_cellCenter[fragmentIdx];
			fieldData_vf_cellCenter[fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (fieldData_vf_nodePosition[fragmentIdx]) {
			delete[] fieldData_vf_nodePosition[fragmentIdx];
			fieldData_vf_nodePosition[fragmentIdx] = 0;
		}
	}
}
