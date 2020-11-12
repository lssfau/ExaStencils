#ifndef EXASTENCILS_USER_USER_H
#define EXASTENCILS_USER_USER_H

#include "cmath"
#include "algorithm"
#include "iostream"
#include "omp.h"

#include "fstream"
#include "sstream"
#include "iomanip"

#include "Communication/Communication.h"
#include "Domain/Domain.h"
#include "Global/Global.h"
#include "Util/Timer.h"

void AdvanceRK2_3();
void ApplyAllBc_3();
void CheckGrid_3();
void CommunicateAll_3();
void InitBath_3();
void InitFieldsAnalytical_3();
void InitTriInfo_3();
void MinDepth_3();
void PrintDebugFields_3();
void SetupPhase_3();
void SolvePhase_3();
void UpdateCTilde_3();
void UpdateUnknowns_3();
void initFieldsWithZero();
int main(int argc, char** argv);

#endif
