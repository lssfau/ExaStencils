#include "fstream"
#include "sstream"
#include "iomanip"

#include "Domain/Domain.h"

void initDomain () {
	{
		/* Read file for level 3 */
		/* Setup connectivity and fields */
		std::ifstream file;
		std::string fileName;
		std::ostringstream string_builder_15;
		string_builder_15 << "BSG/b" << 0 << "_" << 9 << ".block";
		fileName = string_builder_15.str();
		file.open(fileName);
		if (!(file.is_open())) {
			std::cout << "Unable to open file " << " " << fileName << " " << std::endl;
			exit(1);
		}
		std::istringstream iss;
		std::string strBuf;
		readLine(file, iss);
		readLine(file, iss);
		iss >> strBuf >> nFragments;
		readLine(file, iss);
		for (int fragmentIdx = 0; fragmentIdx<nFragments; ++fragmentIdx) {
			isValidForDomain[fragmentIdx] = true;
			int neighborBlockID[4];
			int neighborCommID[4];
			std::string neighborEdge[4];
			int neighborFragID[4];
			readLine(file, iss);
			iss >> strBuf >> fragmentId[fragmentIdx];
			commId[fragmentIdx] = fragmentIdx;
			for (int i = 0; i<4; ++i) {
				readLine(file, iss);
				iss >> strBuf >> neighborBlockID[i] >> neighborCommID[i] >> neighborEdge[i] >> neighborFragID[i];
				if ((neighborBlockID[i]==-1)) {
					neighbor_isValid[i][fragmentIdx] = false;
				} else if ((neighborBlockID[i]==0)) {
					neighbor_isValid[i][fragmentIdx] = true;
					
					neighbor_fragCommId[i][fragmentIdx] = neighborCommID[i];
					/* Iteration offset not implemented */
				} else if ((neighborBlockID[i]!=0)) {
					neighbor_isValid[i][fragmentIdx] = true;
					
					neighbor_fragCommId[i][fragmentIdx] = neighborCommID[i];
					
					/* Iteration offset not implemented */
				}
			}
			for (int i = 0; i<4; ++i) {
				commNeighIdx[i][fragmentIdx] = -1;
				if ((neighborEdge[i]=="W")) {
					commNeighIdx[i][fragmentIdx] = 0;
				}
				if ((neighborEdge[i]=="E")) {
					commNeighIdx[i][fragmentIdx] = 1;
				}
				if ((neighborEdge[i]=="S")) {
					commNeighIdx[i][fragmentIdx] = 2;
				}
				if ((neighborEdge[i]=="N")) {
					commNeighIdx[i][fragmentIdx] = 3;
				}
				if ((neighborEdge[i]=="X")) {
					commTrafoId[i][fragmentIdx] = -1;
					continue;
				}
				if ((((((i==0)&&(commNeighIdx[i][fragmentIdx]==1))||((i==2)&&(commNeighIdx[i][fragmentIdx]==3)))||((i==1)&&(commNeighIdx[i][fragmentIdx]==0)))||((i==3)&&(commNeighIdx[i][fragmentIdx]==2)))) {
					commTrafoId[i][fragmentIdx] = 0;
				}
				if ((((((i==0)&&(commNeighIdx[i][fragmentIdx]==3))||((i==2)&&(commNeighIdx[i][fragmentIdx]==0)))||((i==1)&&(commNeighIdx[i][fragmentIdx]==2)))||((i==3)&&(commNeighIdx[i][fragmentIdx]==1)))) {
					commTrafoId[i][fragmentIdx] = 1;
				}
				if ((((((i==0)&&(commNeighIdx[i][fragmentIdx]==0))||((i==2)&&(commNeighIdx[i][fragmentIdx]==2)))||((i==1)&&(commNeighIdx[i][fragmentIdx]==1)))||((i==3)&&(commNeighIdx[i][fragmentIdx]==3)))) {
					commTrafoId[i][fragmentIdx] = 2;
				}
				if ((((((i==0)&&(commNeighIdx[i][fragmentIdx]==2))||((i==2)&&(commNeighIdx[i][fragmentIdx]==1)))||((i==1)&&(commNeighIdx[i][fragmentIdx]==3)))||((i==3)&&(commNeighIdx[i][fragmentIdx]==0)))) {
					commTrafoId[i][fragmentIdx] = 3;
				}
			}
			if ((neighborBlockID[0]==-1)) {
				boundaryConditionId[0][fragmentIdx] = neighborFragID[0];
				neighFragId[0][fragmentIdx] = -1;
			} else {
				neighFragId[0][fragmentIdx] = neighborFragID[0];
				boundaryConditionId[0][fragmentIdx] = 0;
			}
			if ((neighborBlockID[1]==-1)) {
				boundaryConditionId[1][fragmentIdx] = neighborFragID[1];
				neighFragId[1][fragmentIdx] = -1;
			} else {
				neighFragId[1][fragmentIdx] = neighborFragID[1];
				boundaryConditionId[1][fragmentIdx] = 0;
			}
			if ((neighborBlockID[2]==-1)) {
				boundaryConditionId[2][fragmentIdx] = neighborFragID[2];
				neighFragId[2][fragmentIdx] = -1;
			} else {
				neighFragId[2][fragmentIdx] = neighborFragID[2];
				boundaryConditionId[2][fragmentIdx] = 0;
			}
			if ((neighborBlockID[3]==-1)) {
				boundaryConditionId[3][fragmentIdx] = neighborFragID[3];
				neighFragId[3][fragmentIdx] = -1;
			} else {
				neighFragId[3][fragmentIdx] = neighborFragID[3];
				boundaryConditionId[3][fragmentIdx] = 0;
			}
		}
		setupBuffers();
		initFieldsWithZero();
		for (int fragmentIdx = 0; fragmentIdx<nFragments; ++fragmentIdx) {
			for (int i1 = 0; i1<9; i1 += 1) {
				for (int i0 = 0; i0<9; i0 += 1) {
					readLine(file, iss);
					iss >> fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+28)] >> fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+197)];
				}
			}
		}
		file.close();
		exchvf_nodePosition_ghost_a_a_a_a_a_a_a_a_3(0);
		for (int fragmentIdx = 0; fragmentIdx<nFragments; ++fragmentIdx) {
			if (!(neighbor_isValid[0][fragmentIdx])) {
				/* 0 */
				for (int i1 = 3; i1<10; i1 += 1) {
					for (int i0 = 0; i0<2; i0 += 1) {
						/* Non-Corner Boundaries */
						float tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+15)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)-11)]));
						float ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+184)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+158)]));
						float nx = ty;
						float ny = (-tx);
						int ghostId = ((i0+i1)-(i1+2));
						fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0)] = ((ghostId*nx)+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+2)]);
						fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+169)] = ((ghostId*ny)+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+171)]);
					}
				}
				{
					/* Corners */
					float tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][54]-fieldData_vf_nodePosition[fragmentIdx][28]));
					float ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][223]-fieldData_vf_nodePosition[fragmentIdx][197]));
					float nx = ty;
					float ny = (-tx);
					fieldData_vf_nodePosition[fragmentIdx][27] = (fieldData_vf_nodePosition[fragmentIdx][28]-nx);
					fieldData_vf_nodePosition[fragmentIdx][196] = (fieldData_vf_nodePosition[fragmentIdx][197]-ny);
					tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][132]-fieldData_vf_nodePosition[fragmentIdx][106]));
					ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][301]-fieldData_vf_nodePosition[fragmentIdx][275]));
					nx = ty;
					ny = (-tx);
					fieldData_vf_nodePosition[fragmentIdx][131] = (fieldData_vf_nodePosition[fragmentIdx][132]-nx);
					fieldData_vf_nodePosition[fragmentIdx][300] = (fieldData_vf_nodePosition[fragmentIdx][301]-ny);
				}
			}
			if (!(neighbor_isValid[1][fragmentIdx])) {
				/* 1 */
				for (int i1 = 3; i1<10; i1 += 1) {
					for (int i0 = 11; i0<13; i0 += 1) {
						/* Non-Corner Boundaries */
						float tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+23)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)-3)]));
						float ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][((13*i1)+192)]-fieldData_vf_nodePosition[fragmentIdx][((13*i1)+166)]));
						float nx = ty;
						float ny = (-tx);
						int ghostId = ((i0+i1)-(i1+10));
						fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0)] = ((ghostId*nx)+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+10)]);
						fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+169)] = ((ghostId*ny)+fieldData_vf_nodePosition[fragmentIdx][((13*i1)+179)]);
					}
				}
				{
					/* Corners */
					float tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][62]-fieldData_vf_nodePosition[fragmentIdx][36]));
					float ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][231]-fieldData_vf_nodePosition[fragmentIdx][205]));
					float nx = ty;
					float ny = (-tx);
					fieldData_vf_nodePosition[fragmentIdx][37] = (nx+fieldData_vf_nodePosition[fragmentIdx][36]);
					fieldData_vf_nodePosition[fragmentIdx][206] = (ny+fieldData_vf_nodePosition[fragmentIdx][205]);
					tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][140]-fieldData_vf_nodePosition[fragmentIdx][114]));
					ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][309]-fieldData_vf_nodePosition[fragmentIdx][283]));
					nx = ty;
					ny = (-tx);
					fieldData_vf_nodePosition[fragmentIdx][141] = (nx+fieldData_vf_nodePosition[fragmentIdx][140]);
					fieldData_vf_nodePosition[fragmentIdx][310] = (ny+fieldData_vf_nodePosition[fragmentIdx][309]);
				}
			}
			if (!(neighbor_isValid[2][fragmentIdx])) {
				/* 2 */
				for (int i1 = 0; i1<2; i1 += 1) {
					for (int i0 = 3; i0<10; i0 += 1) {
						/* Non-Corner Boundaries */
						float tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][(i0+27)]-fieldData_vf_nodePosition[fragmentIdx][(i0+25)]));
						float ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][(i0+196)]-fieldData_vf_nodePosition[fragmentIdx][(i0+194)]));
						float nx = ty;
						float ny = (-tx);
						int ghostId = ((i0+2)-(i0+i1));
						fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0)] = ((ghostId*nx)+fieldData_vf_nodePosition[fragmentIdx][(i0+26)]);
						fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+169)] = ((ghostId*ny)+fieldData_vf_nodePosition[fragmentIdx][(i0+195)]);
					}
				}
				{
					/* Corners */
					float tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][30]-fieldData_vf_nodePosition[fragmentIdx][28]));
					float ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][199]-fieldData_vf_nodePosition[fragmentIdx][197]));
					float nx = ty;
					float ny = (-tx);
					fieldData_vf_nodePosition[fragmentIdx][15] = (fieldData_vf_nodePosition[fragmentIdx][28]-nx);
					fieldData_vf_nodePosition[fragmentIdx][184] = (fieldData_vf_nodePosition[fragmentIdx][197]-ny);
					tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][36]-fieldData_vf_nodePosition[fragmentIdx][34]));
					ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][205]-fieldData_vf_nodePosition[fragmentIdx][203]));
					nx = ty;
					ny = (-tx);
					fieldData_vf_nodePosition[fragmentIdx][23] = (fieldData_vf_nodePosition[fragmentIdx][36]-nx);
					fieldData_vf_nodePosition[fragmentIdx][192] = (fieldData_vf_nodePosition[fragmentIdx][205]-ny);
				}
			}
			if (!(neighbor_isValid[3][fragmentIdx])) {
				/* 3 */
				for (int i1 = 11; i1<13; i1 += 1) {
					for (int i0 = 3; i0<10; i0 += 1) {
						/* Non-Corner Boundaries */
						float tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][(i0+131)]-fieldData_vf_nodePosition[fragmentIdx][(i0+129)]));
						float ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][(i0+300)]-fieldData_vf_nodePosition[fragmentIdx][(i0+298)]));
						float nx = ty;
						float ny = (-tx);
						int ghostId = ((i0+10)-(i0+i1));
						fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0)] = ((ghostId*nx)+fieldData_vf_nodePosition[fragmentIdx][(i0+130)]);
						fieldData_vf_nodePosition[fragmentIdx][((13*i1)+i0+169)] = ((ghostId*ny)+fieldData_vf_nodePosition[fragmentIdx][(i0+299)]);
					}
				}
				{
					/* Corners */
					float tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][134]-fieldData_vf_nodePosition[fragmentIdx][132]));
					float ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][303]-fieldData_vf_nodePosition[fragmentIdx][301]));
					float nx = ty;
					float ny = (-tx);
					fieldData_vf_nodePosition[fragmentIdx][145] = (nx+fieldData_vf_nodePosition[fragmentIdx][132]);
					fieldData_vf_nodePosition[fragmentIdx][314] = (ny+fieldData_vf_nodePosition[fragmentIdx][301]);
					tx = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][140]-fieldData_vf_nodePosition[fragmentIdx][138]));
					ty = (0.5f*(fieldData_vf_nodePosition[fragmentIdx][309]-fieldData_vf_nodePosition[fragmentIdx][307]));
					nx = ty;
					ny = (-tx);
					fieldData_vf_nodePosition[fragmentIdx][153] = (nx+fieldData_vf_nodePosition[fragmentIdx][140]);
					fieldData_vf_nodePosition[fragmentIdx][322] = (ny+fieldData_vf_nodePosition[fragmentIdx][309]);
				}
			}
		}
		/* Adapt ghost layers by repositioning knots */
		for (int fragmentIdx = 0; fragmentIdx<nFragments; ++fragmentIdx) {
			if (((commTrafoId[0][fragmentIdx]==1)||(commTrafoId[0][fragmentIdx]==3))) {
				for (int iDir = 12; iDir>0; --iDir) {
					fieldData_vf_nodePosition[fragmentIdx][((13*iDir)+1)] = fieldData_vf_nodePosition[fragmentIdx][((13*iDir)-12)];
					fieldData_vf_nodePosition[fragmentIdx][((13*iDir)+170)] = fieldData_vf_nodePosition[fragmentIdx][((13*iDir)+157)];
				}
			}
			if (((commTrafoId[1][fragmentIdx]==1)||(commTrafoId[1][fragmentIdx]==3))) {
				for (int iDir = 0; iDir<12; ++iDir) {
					fieldData_vf_nodePosition[fragmentIdx][((13*iDir)+11)] = fieldData_vf_nodePosition[fragmentIdx][((13*iDir)+24)];
					fieldData_vf_nodePosition[fragmentIdx][((13*iDir)+180)] = fieldData_vf_nodePosition[fragmentIdx][((13*iDir)+193)];
				}
			}
			if (((commTrafoId[2][fragmentIdx]==1)||(commTrafoId[2][fragmentIdx]==3))) {
				for (int iDir = 12; iDir>0; --iDir) {
					fieldData_vf_nodePosition[fragmentIdx][(iDir+13)] = fieldData_vf_nodePosition[fragmentIdx][(iDir+12)];
					fieldData_vf_nodePosition[fragmentIdx][(iDir+182)] = fieldData_vf_nodePosition[fragmentIdx][(iDir+181)];
				}
			}
			if (((commTrafoId[3][fragmentIdx]==1)||(commTrafoId[3][fragmentIdx]==3))) {
				for (int iDir = 0; iDir<12; ++iDir) {
					fieldData_vf_nodePosition[fragmentIdx][(iDir+143)] = fieldData_vf_nodePosition[fragmentIdx][(iDir+144)];
					fieldData_vf_nodePosition[fragmentIdx][(iDir+312)] = fieldData_vf_nodePosition[fragmentIdx][(iDir+313)];
				}
			}
		}
	}
}
