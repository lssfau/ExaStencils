#ifndef EXASTENCILS_GLOBAL_GLOBAL_H
#define EXASTENCILS_GLOBAL_GLOBAL_H

#include "algorithm"
#include "omp.h"
#include "iostream"

#include "fstream"
#include "sstream"
#include "iomanip"

#include "Util/Stopwatch.h"
#include "Util/Util.h"

void destroyGlobals();
void initGlobals();
void readParameterFile(std::string fileName);
void setupBuffers();
extern int boundaryConditionId[4][28];
extern int commId[28];
extern int commNeighIdx[4][28];
extern int commTrafoId[4][28];
extern double dt;
extern double errceta_3;
extern double errcu_3;
extern double errcv_3;
extern double* fieldData_ETRI[28];
extern double* fieldData_UNRI[28];
extern double* fieldData_UTRI[28];
extern double* fieldData_adaptivity_nlockLower0[28];
extern double* fieldData_adaptivity_nlockUpper0[28];
extern double* fieldData_bLower0[28];
extern double* fieldData_bLower1[28];
extern double* fieldData_bLower2[28];
extern double* fieldData_bLower3[28];
extern double* fieldData_bUpper0[28];
extern double* fieldData_bUpper1[28];
extern double* fieldData_bUpper2[28];
extern double* fieldData_bUpper3[28];
extern double* fieldData_bath[28];
extern double* fieldData_cetaEdgeDLower0[28];
extern double* fieldData_cetaEdgeDUpper0[28];
extern double* fieldData_cetaEdgeHLower0[28];
extern double* fieldData_cetaEdgeHUpper0[28];
extern double* fieldData_cetaEdgeVLower0[28];
extern double* fieldData_cetaEdgeVUpper0[28];
extern double* fieldData_cetaLower0[28];
extern double* fieldData_cetaNewLower0[28];
extern double* fieldData_cetaNewUpper0[28];
extern double* fieldData_cetaOldLower0[28];
extern double* fieldData_cetaOldUpper0[28];
extern double* fieldData_cetaUpper0[28];
extern double* fieldData_cuEdgeDLower0[28];
extern double* fieldData_cuEdgeDUpper0[28];
extern double* fieldData_cuEdgeHLower0[28];
extern double* fieldData_cuEdgeHUpper0[28];
extern double* fieldData_cuEdgeVLower0[28];
extern double* fieldData_cuEdgeVUpper0[28];
extern double* fieldData_cuLower0[28];
extern double* fieldData_cuNewLower0[28];
extern double* fieldData_cuNewUpper0[28];
extern double* fieldData_cuOldLower0[28];
extern double* fieldData_cuOldUpper0[28];
extern double* fieldData_cuTildeLower0[28];
extern double* fieldData_cuTildeUpper0[28];
extern double* fieldData_cuUpper0[28];
extern double* fieldData_cvEdgeDLower0[28];
extern double* fieldData_cvEdgeDUpper0[28];
extern double* fieldData_cvEdgeHLower0[28];
extern double* fieldData_cvEdgeHUpper0[28];
extern double* fieldData_cvEdgeVLower0[28];
extern double* fieldData_cvEdgeVUpper0[28];
extern double* fieldData_cvLower0[28];
extern double* fieldData_cvNewLower0[28];
extern double* fieldData_cvNewUpper0[28];
extern double* fieldData_cvOldLower0[28];
extern double* fieldData_cvOldUpper0[28];
extern double* fieldData_cvTildeLower0[28];
extern double* fieldData_cvTildeUpper0[28];
extern double* fieldData_cvUpper0[28];
extern double* fieldData_detBInvLower0[28];
extern double* fieldData_detBInvUpper0[28];
extern double* fieldData_detBLower0[28];
extern double* fieldData_detBUpper0[28];
extern double* fieldData_edgeLenDiagonal0[28];
extern double* fieldData_edgeLenHorizontal0[28];
extern double* fieldData_edgeLenVertical0[28];
extern double* fieldData_local_orderLower0[28];
extern double* fieldData_local_orderUpper0[28];
extern double* fieldData_normalDiagonal0[28];
extern double* fieldData_normalDiagonal1[28];
extern double* fieldData_normalHorizontal0[28];
extern double* fieldData_normalHorizontal1[28];
extern double* fieldData_normalVertical0[28];
extern double* fieldData_normalVertical1[28];
extern double* fieldData_sqrtDetBLower0[28];
extern double* fieldData_sqrtDetBUpper0[28];
extern double* fieldData_sqrtInvDetBLower0[28];
extern double* fieldData_sqrtInvDetBUpper0[28];
extern double* fieldData_tidal[28];
extern double* fieldData_vf_cellCenter[28];
extern double* fieldData_vf_nodePosition[28];
extern size_t fragmentId[28];
extern bool isValidForDomain[28];
extern int it;
extern int nFragments;
extern int neighFragId[4][28];
extern int neighbor_fragCommId[4][28];
extern bool neighbor_isValid[4][28];
extern int print_count;
extern double t;
extern StopWatch timer_AdvanceRK2;
extern StopWatch timer_ApplyAllBc;
extern StopWatch timer_CommunicateAll;
extern StopWatch timer_InitFieldsAnalytical;
extern StopWatch timer_MinDepth;
extern StopWatch timer_PrintDebugFields;
extern StopWatch timer_SetupPhase;
extern StopWatch timer_SolvePhase;
extern StopWatch timer_UpdateCTilde;
extern StopWatch timer_UpdateUnknowns;
extern StopWatch timer_print;
extern StopWatch timer_setup;
extern StopWatch timer_timeLoop;

#endif
