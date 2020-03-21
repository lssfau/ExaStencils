#ifndef EXASTENCILS_GLOBAL_GLOBAL_H
#define EXASTENCILS_GLOBAL_GLOBAL_H

#include "algorithm"

#include "Util/Stopwatch.h"

void initGlobals();
using __matrix_double_6_6_t = double[36];
using __matrix_double_8_8_t = double[64];
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
