#ifndef EXASTENCILS_GLOBAL_GLOBAL_H
#define EXASTENCILS_GLOBAL_GLOBAL_H

#include "algorithm"

#include "Util/Stopwatch.h"

void initGlobals();
using __matrix_int_4_4_t = int[16];
using __matrix_double_4_4_t = double[16];
using __matrix_int_3_3_t = int[9];
using __matrix_int_1_2_t = int[2];
using __matrix_int_3_1_t = int[3];
using __matrix_int_3_2_t = int[6];
extern int commId;
extern size_t fragmentId;
extern int fragmentIndex_0;
extern int fragmentIndex_1;
extern double fragmentPosBegin_0;
extern double fragmentPosBegin_1;
extern double fragmentPosEnd_0;
extern double fragmentPosEnd_1;
extern double fragmentPos_0;
extern double fragmentPos_1;

#endif
