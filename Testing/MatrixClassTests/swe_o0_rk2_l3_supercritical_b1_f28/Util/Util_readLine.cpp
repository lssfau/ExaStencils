#include "fstream"
#include "sstream"
#include "iomanip"

#include "Util/Util.h"

bool readLine (std::ifstream& ifs, std::istringstream& iss) {
	std::string line;
	std::getline(ifs, line);
	/* Delete comments and spaces at beginning and end of string */
	if ((line.find("#")!=std::string::npos)) {
		line.erase(line.find("#"));
	}
	line.erase(line.begin(), std::find_if(line.begin(), line.end(), std::not1(std::ptr_fun<int,int>(std::isspace))));
	line.erase(std::find_if(line.rbegin(), line.rend(), std::not1(std::ptr_fun<int,int>(std::isspace))).base(), line.end());
	while ((line.size()==0)) {
		if (!(std::getline(ifs, line))) {
			return false;
		}
		/* Delete comments and spaces at beginning and end of string */
		if ((line.find("#")!=std::string::npos)) {
			line.erase(line.find("#"));
		}
		line.erase(line.begin(), std::find_if(line.begin(), line.end(), std::not1(std::ptr_fun<int,int>(std::isspace))));
		line.erase(std::find_if(line.rbegin(), line.rend(), std::not1(std::ptr_fun<int,int>(std::isspace))).base(), line.end());
	}
	iss.clear();
	iss.str(line);
	return true;
}
