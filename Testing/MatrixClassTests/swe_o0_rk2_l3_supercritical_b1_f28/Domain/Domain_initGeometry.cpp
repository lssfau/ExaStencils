#include "fstream"
#include "sstream"
#include "iomanip"

#include "Domain/Domain.h"

void initGeometry () {
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = -1; i1<9; i1 += 1) {
				for (int i0 = -1; i0<9; i0 += 1) {
					fieldData_vf_cellCenter[fragmentIdx][((12*i1)+i0+26)] = (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+29)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+41)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+42)]));
					fieldData_vf_cellCenter[fragmentIdx][((12*i1)+i0+170)] = (0.25*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+198)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+210)]+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+211)]));
				}
			}
		}
	}
}
