#include "fstream"
#include "sstream"
#include "iomanip"

#include "Communication/Communication.h"

void waitForFlag (volatile bool* flag) {
	while (!((*(flag)))) {
		
	}
	(*(flag)) = false;
}
