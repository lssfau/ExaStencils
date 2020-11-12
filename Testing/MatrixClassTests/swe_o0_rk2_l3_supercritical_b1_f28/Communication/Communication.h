#ifndef EXASTENCILS_COMMUNICATION_COMMUNICATION_H
#define EXASTENCILS_COMMUNICATION_COMMUNICATION_H

#include "cmath"
#include "algorithm"
#include "omp.h"

#include "fstream"
#include "sstream"
#include "iomanip"

#include "Global/Global.h"
#include "User/User.h"

void exchbath_all_a_a_a_a_3(int slot);
void exchcetaLower0_all_a_a_a_a_3(int slot);
void exchcetaUpper0_all_a_a_a_a_3(int slot);
void exchcuLower0_all_a_a_a_a_3(int slot);
void exchcuTildeLower0_all_a_a_a_a_3(int slot);
void exchcuTildeUpper0_all_a_a_a_a_3(int slot);
void exchcuUpper0_all_a_a_a_a_3(int slot);
void exchcvLower0_all_a_a_a_a_3(int slot);
void exchcvTildeLower0_all_a_a_a_a_3(int slot);
void exchcvTildeUpper0_all_a_a_a_a_3(int slot);
void exchcvUpper0_all_a_a_a_a_3(int slot);
void exchvf_nodePosition_ghost_a_a_a_a_a_a_a_a_3(int slot);
void waitForFlag(volatile bool* flag);

#endif
