#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void PrintDebugFields_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_PrintDebugFields;
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i00_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	std::string filename_ceta_Lower_0;
	std::ostringstream string_builder_02;
	string_builder_02 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_ceta_Lower_0_-_" << print_count << ".txt";
	filename_ceta_Lower_0 = string_builder_02.str();
	std::ofstream fieldPrintStream_01(filename_ceta_Lower_0, std::ios::trunc);
	fieldPrintStream_01 << "x,y,z,s0" << std::endl;
	fieldPrintStream_01 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_01 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_01.close();
	std::string filename_ceta_Upper_0;
	std::ostringstream string_builder_03;
	string_builder_03 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_ceta_Upper_0_-_" << print_count << ".txt";
	filename_ceta_Upper_0 = string_builder_03.str();
	std::ofstream fieldPrintStream_02(filename_ceta_Upper_0, std::ios::trunc);
	fieldPrintStream_02 << "x,y,z,s0" << std::endl;
	fieldPrintStream_02 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_02 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_02.close();
	std::string filename_cu_Lower_0;
	std::ostringstream string_builder_04;
	string_builder_04 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_cu_Lower_0_-_" << print_count << ".txt";
	filename_cu_Lower_0 = string_builder_04.str();
	std::ofstream fieldPrintStream_03(filename_cu_Lower_0, std::ios::trunc);
	fieldPrintStream_03 << "x,y,z,s0" << std::endl;
	fieldPrintStream_03 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_03 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_03.close();
	std::string filename_cu_Upper_0;
	std::ostringstream string_builder_05;
	string_builder_05 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_cu_Upper_0_-_" << print_count << ".txt";
	filename_cu_Upper_0 = string_builder_05.str();
	std::ofstream fieldPrintStream_04(filename_cu_Upper_0, std::ios::trunc);
	fieldPrintStream_04 << "x,y,z,s0" << std::endl;
	fieldPrintStream_04 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_04 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_04.close();
	std::string filename_cv_Lower_0;
	std::ostringstream string_builder_06;
	string_builder_06 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_cv_Lower_0_-_" << print_count << ".txt";
	filename_cv_Lower_0 = string_builder_06.str();
	std::ofstream fieldPrintStream_05(filename_cv_Lower_0, std::ios::trunc);
	fieldPrintStream_05 << "x,y,z,s0" << std::endl;
	fieldPrintStream_05 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_05 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_05.close();
	std::string filename_cv_Upper_0;
	std::ostringstream string_builder_07;
	string_builder_07 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_cv_Upper_0_-_" << print_count << ".txt";
	filename_cv_Upper_0 = string_builder_07.str();
	std::ofstream fieldPrintStream_06(filename_cv_Upper_0, std::ios::trunc);
	fieldPrintStream_06 << "x,y,z,s0" << std::endl;
	fieldPrintStream_06 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_06 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_06.close();
	std::string filename_cuTilde_Lower_0;
	std::ostringstream string_builder_08;
	string_builder_08 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_cuTilde_Lower_0_-_" << print_count << ".txt";
	filename_cuTilde_Lower_0 = string_builder_08.str();
	std::ofstream fieldPrintStream_07(filename_cuTilde_Lower_0, std::ios::trunc);
	fieldPrintStream_07 << "x,y,z,s0" << std::endl;
	fieldPrintStream_07 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_07 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_07.close();
	std::string filename_cuTilde_Upper_0;
	std::ostringstream string_builder_09;
	string_builder_09 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_cuTilde_Upper_0_-_" << print_count << ".txt";
	filename_cuTilde_Upper_0 = string_builder_09.str();
	std::ofstream fieldPrintStream_08(filename_cuTilde_Upper_0, std::ios::trunc);
	fieldPrintStream_08 << "x,y,z,s0" << std::endl;
	fieldPrintStream_08 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_08 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_08.close();
	std::string filename_cvTilde_Lower_0;
	std::ostringstream string_builder_10;
	string_builder_10 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_cvTilde_Lower_0_-_" << print_count << ".txt";
	filename_cvTilde_Lower_0 = string_builder_10.str();
	std::ofstream fieldPrintStream_09(filename_cvTilde_Lower_0, std::ios::trunc);
	fieldPrintStream_09 << "x,y,z,s0" << std::endl;
	fieldPrintStream_09 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_09 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_09.close();
	std::string filename_cvTilde_Upper_0;
	std::ostringstream string_builder_11;
	string_builder_11 << "../../data/swe_o0_rk2_l3_supercritical_b1_f28_cvTilde_Upper_0_-_" << print_count << ".txt";
	filename_cvTilde_Upper_0 = string_builder_11.str();
	std::ofstream fieldPrintStream_10(filename_cvTilde_Upper_0, std::ios::trunc);
	fieldPrintStream_10 << "x,y,z,s0" << std::endl;
	fieldPrintStream_10 << std::scientific << std::setprecision(10);
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldPrintStream_10 << std::defaultfloat << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)])) << "," << (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)])) << "," << std::scientific << fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+11)] << "," << std::endl;
				}
			}
		}
	}
	fieldPrintStream_10.close();
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_PrintDebugFields;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
