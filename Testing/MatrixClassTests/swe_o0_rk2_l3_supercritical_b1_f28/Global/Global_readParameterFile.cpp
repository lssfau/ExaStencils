#include "fstream"
#include "sstream"
#include "iomanip"

#include "Global/Global.h"

void readParameterFile (std::string fileName) {
	std::ifstream file;
	file.open(fileName);
	if (!(file.is_open())) {
		std::cout << "Unable to open file " << " " << fileName << " " << std::endl;
		exit(1);
	}
	std::istringstream iss;
	std::string name;
	std::string value;
	while (readLine(file, iss)) {
		iss >> name >> value;
		std::istringstream issVal(value);
		if ((name=="dt")) {
			issVal >> dt;
			continue;
		}
		if ((name=="t")) {
			issVal >> t;
			continue;
		}
		if ((name=="print_count")) {
			issVal >> print_count;
			continue;
		}
		if ((name=="it")) {
			issVal >> it;
			continue;
		}
		if ((name=="errceta_3")) {
			issVal >> errceta_3;
			continue;
		}
		if ((name=="errcu_3")) {
			issVal >> errcu_3;
			continue;
		}
		if ((name=="errcv_3")) {
			issVal >> errcv_3;
			continue;
		}
	}
}
