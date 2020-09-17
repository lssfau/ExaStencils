#include "cstdlib"

#include "fstream"
#include "sstream"
#include "iomanip"

#include "Global/Global.h"

void initGlobals () {
	for (int neighborIdx = 0; neighborIdx<4; ++neighborIdx) {
		for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
			boundaryConditionId[neighborIdx][fragmentIdx] = 0;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		commId[fragmentIdx] = -1;
	}
	for (int neighborIdx = 0; neighborIdx<4; ++neighborIdx) {
		for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
			commNeighIdx[neighborIdx][fragmentIdx] = -1;
		}
	}
	for (int neighborIdx = 0; neighborIdx<4; ++neighborIdx) {
		for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
			commTrafoId[neighborIdx][fragmentIdx] = -1;
		}
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_ETRI[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_UNRI[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_UTRI[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_adaptivity_nlockLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_adaptivity_nlockUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_bLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_bLower1[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_bLower2[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_bLower3[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_bUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_bUpper1[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_bUpper2[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_bUpper3[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_bath[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaEdgeDLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaEdgeDUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaEdgeHLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaEdgeHUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaEdgeVLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaEdgeVUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaNewLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaNewUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaOldLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaOldUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cetaUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuEdgeDLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuEdgeDUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuEdgeHLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuEdgeHUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuEdgeVLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuEdgeVUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuNewLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuNewUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuOldLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuOldUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuTildeLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuTildeUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cuUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvEdgeDLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvEdgeDUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvEdgeHLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvEdgeHUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvEdgeVLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvEdgeVUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvNewLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvNewUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvOldLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvOldUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvTildeLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvTildeUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_cvUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_detBInvLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_detBInvUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_detBLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_detBUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_edgeLenDiagonal0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_edgeLenHorizontal0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_edgeLenVertical0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_local_orderLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_local_orderUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_normalDiagonal0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_normalDiagonal1[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_normalHorizontal0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_normalHorizontal1[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_normalVertical0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_normalVertical1[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_sqrtDetBLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_sqrtDetBUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_sqrtInvDetBLower0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_sqrtInvDetBUpper0[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_tidal[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_vf_cellCenter[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fieldData_vf_nodePosition[fragmentIdx] = 0;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		fragmentId[fragmentIdx] = -1;
	}
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		isValidForDomain[fragmentIdx] = false;
	}
	nFragments = 28;
	for (int neighborIdx = 0; neighborIdx<4; ++neighborIdx) {
		for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
			neighFragId[neighborIdx][fragmentIdx] = -1;
		}
	}
	for (int neighborIdx = 0; neighborIdx<4; ++neighborIdx) {
		for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
			neighbor_fragCommId[neighborIdx][fragmentIdx] = -1;
		}
	}
	for (int neighborIdx = 0; neighborIdx<4; ++neighborIdx) {
		for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
			neighbor_isValid[neighborIdx][fragmentIdx] = false;
		}
	}
	timer_AdvanceRK2.timerName = "AdvanceRK2";
	timer_ApplyAllBc.timerName = "ApplyAllBc";
	timer_CommunicateAll.timerName = "CommunicateAll";
	timer_InitFieldsAnalytical.timerName = "InitFieldsAnalytical";
	timer_MinDepth.timerName = "MinDepth";
	timer_PrintDebugFields.timerName = "PrintDebugFields";
	timer_SetupPhase.timerName = "SetupPhase";
	timer_SolvePhase.timerName = "SolvePhase";
	timer_UpdateCTilde.timerName = "UpdateCTilde";
	timer_UpdateUnknowns.timerName = "UpdateUnknowns";
	timer_print.timerName = "print";
	timer_setup.timerName = "setup";
	timer_timeLoop.timerName = "timeLoop";
}
