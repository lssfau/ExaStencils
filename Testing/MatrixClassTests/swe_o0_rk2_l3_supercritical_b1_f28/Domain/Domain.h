#ifndef EXASTENCILS_DOMAIN_DOMAIN_H
#define EXASTENCILS_DOMAIN_DOMAIN_H

#include "omp.h"

#include "fstream"
#include "sstream"
#include "iomanip"

#include "Communication/Communication.h"
#include "Global/Global.h"

void initDomain();
void initGeometry();

#endif
