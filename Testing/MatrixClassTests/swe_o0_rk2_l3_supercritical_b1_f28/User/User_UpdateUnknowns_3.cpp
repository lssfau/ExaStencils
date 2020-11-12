#include "fstream"
#include "sstream"
#include "iomanip"

#include "User/User.h"

void UpdateUnknowns_3 () {
	/* -.-.-.- inlined startTimer -.-.-.- */
	StopWatch& _i00_stopWatch = timer_UpdateUnknowns;
	if ((0==_i00_stopWatch.numEntries)) {
		
		_i00_stopWatch.timerStarted = std::chrono::high_resolution_clock::now();
		_i00_stopWatch.lastTimeMeasured = std::chrono::nanoseconds::zero();
	}
	(++_i00_stopWatch.numEntries);
	/* =^=^=^=^= end startTimer =^=^=^=^= */
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					double tmp0 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp1 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp2 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp3 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp4 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp5 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp6 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp7 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp8 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp9 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp10 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp11 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp12 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp13 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp14 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp15 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp16 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp17 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp18 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp19 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]);
					double lam0 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp10+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]))),(tmp11+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])))}),0.0})))+std::max({std::fabs(((fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)]*tmp16)+(fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)]*tmp18))),std::fabs(((fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)]*tmp17)+(fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)]*tmp19)))}));
					double tmp20 = ((fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)]*tmp2)+(fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)]*tmp4));
					double tmp21 = ((fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)]*tmp3)+(fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)]*tmp5));
					double tmp22 = ((((tmp2*tmp6)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+13)]*tmp0)+(0.08*tmp0*tmp0))*fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)]*tmp2*tmp8)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+23)]-fieldData_bath[fragmentIdx][((11*i1)+i0+13)])*fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)]*tmp0));
					double tmp23 = ((((tmp3*tmp7)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+13)]*tmp1)+(0.08*tmp1*tmp1))*fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)]*tmp3*tmp9)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+23)]-fieldData_bath[fragmentIdx][((11*i1)+i0+13)])*fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)]*tmp1));
					double tmp24 = ((((tmp4*tmp8)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+13)]*tmp0)+(0.08*tmp0*tmp0))*fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)]*tmp4*tmp6)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+23)]-fieldData_bath[fragmentIdx][((11*i1)+i0+13)])*fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)]*tmp0));
					double tmp25 = ((((tmp5*tmp9)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+13)]*tmp1)+(0.08*tmp1*tmp1))*fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalDiagonal0[fragmentIdx][((10*i1)+i0+11)]*tmp5*tmp7)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+23)]-fieldData_bath[fragmentIdx][((11*i1)+i0+13)])*fieldData_normalDiagonal1[fragmentIdx][((10*i1)+i0+11)]*tmp1));
					fieldData_cetaEdgeDLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp20+tmp21+((tmp0-tmp1)*lam0))*fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cuEdgeDLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp22+tmp23+((tmp2-tmp3)*lam0))*fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvEdgeDLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp24+tmp25+((tmp4-tmp5)*lam0))*fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cetaEdgeDUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp1-tmp0)*lam0)-(tmp20+tmp21))*fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cuEdgeDUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp3-tmp2)*lam0)-(tmp22+tmp23))*fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvEdgeDUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp5-tmp4)*lam0)-(tmp24+tmp25))*fieldData_edgeLenDiagonal0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 1; i0<8; i0 += 1) {
					double tmp26 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp27 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp28 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp29 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp30 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp31 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp32 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp33 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp34 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp35 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp36 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp37 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp38 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp39 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp40 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp41 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp42 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp43 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp44 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp45 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double lam1 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp36+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]))),(tmp37+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])))}),0.0})))+std::max({std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp42)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp44))),std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp43)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp45)))}));
					double tmp46 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp28)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp30));
					double tmp47 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp29)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp31));
					double tmp48 = ((((tmp28*tmp32)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp26)+(0.08*tmp26*tmp26))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp28*tmp34)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp26));
					double tmp49 = ((((tmp29*tmp33)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp27)+(0.08*tmp27*tmp27))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp29*tmp35)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp27));
					double tmp50 = ((((tmp30*tmp34)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp26)+(0.08*tmp26*tmp26))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp30*tmp32)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp26));
					double tmp51 = ((((tmp31*tmp35)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp27)+(0.08*tmp27*tmp27))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp31*tmp33)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp27));
					fieldData_cetaEdgeVLower0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(tmp46+tmp47+((tmp26-tmp27)*lam1))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_cuEdgeVLower0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(tmp48+tmp49+((tmp28-tmp29)*lam1))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_cvEdgeVLower0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(tmp50+tmp51+((tmp30-tmp31)*lam1))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_cetaEdgeVUpper0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(((tmp27-tmp26)*lam1)-(tmp46+tmp47))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_cuEdgeVUpper0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(((tmp29-tmp28)*lam1)-(tmp48+tmp49))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_cvEdgeVUpper0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(((tmp31-tmp30)*lam1)-(tmp50+tmp51))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
				}
			}
		}
		if ((((commNeighIdx[0][fragmentIdx]<0)||(commNeighIdx[0][fragmentIdx]==0))||(commNeighIdx[0][fragmentIdx]==1))) {
			if (isValidForDomain[fragmentIdx]) {
				for (int i1 = 0; i1<8; i1 += 1) {
					for (int i0 = 0; i0<1; i0 += 1) {
						double tmp52 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp53 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp54 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp55 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp56 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp57 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp58 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp59 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp60 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp61 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp62 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp63 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp64 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp65 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp66 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp67 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp68 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp69 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp70 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp71 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double lam1 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp62+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]))),(tmp63+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])))}),0.0})))+std::max({std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp68)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp70))),std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp69)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp71)))}));
						double tmp72 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp54)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp56));
						double tmp73 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp55)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp57));
						double tmp74 = ((((tmp54*tmp58)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp52)+(0.08*tmp52*tmp52))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp54*tmp60)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp52));
						double tmp75 = ((((tmp55*tmp59)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp53)+(0.08*tmp53*tmp53))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp55*tmp61)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp53));
						double tmp76 = ((((tmp56*tmp60)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp52)+(0.08*tmp52*tmp52))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp56*tmp58)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp52));
						double tmp77 = ((((tmp57*tmp61)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp53)+(0.08*tmp53*tmp53))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp57*tmp59)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp53));
						fieldData_cetaEdgeVLower0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(tmp72+tmp73+((tmp52-tmp53)*lam1))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
						fieldData_cuEdgeVLower0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(tmp74+tmp75+((tmp54-tmp55)*lam1))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
						fieldData_cvEdgeVLower0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(tmp76+tmp77+((tmp56-tmp57)*lam1))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					}
				}
			}
		} else if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<1; i0 += 1) {
					double tmp78 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp79 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp80 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp81 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp82 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp83 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp84 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp85 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp86 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp87 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp88 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp89 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp90 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp91 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp92 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp93 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp94 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp95 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp96 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp97 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double lam1 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp88+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]))),(tmp89+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])))}),0.0})))+std::max({std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp94)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp96))),std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp95)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp97)))}));
					double tmp98 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp80)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp82));
					double tmp99 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp81)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp83));
					double tmp100 = ((((tmp80*tmp84)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp78)+(0.08*tmp78*tmp78))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp80*tmp86)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp78));
					double tmp101 = ((((tmp81*tmp85)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp79)+(0.08*tmp79*tmp79))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp81*tmp87)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp79));
					double tmp102 = ((((tmp82*tmp86)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp78)+(0.08*tmp78*tmp78))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp82*tmp84)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp78));
					double tmp103 = ((((tmp83*tmp87)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp79)+(0.08*tmp79*tmp79))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp83*tmp85)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp79));
					fieldData_cetaEdgeVLower0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(tmp98+tmp99+((tmp78-tmp79)*lam1))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_cuEdgeVLower0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(tmp100+tmp101+((tmp80-tmp81)*lam1))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_cvEdgeVLower0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(tmp102+tmp103+((tmp82-tmp83)*lam1))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
				}
			}
		}
		if ((((commNeighIdx[1][fragmentIdx]<0)||(commNeighIdx[1][fragmentIdx]==0))||(commNeighIdx[1][fragmentIdx]==1))) {
			if (isValidForDomain[fragmentIdx]) {
				for (int i1 = 0; i1<8; i1 += 1) {
					for (int i0 = 8; i0<9; i0 += 1) {
						double tmp104 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp105 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp106 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp107 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp108 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp109 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp110 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp111 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp112 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp113 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp114 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp115 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp116 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp117 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp118 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp119 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp120 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp121 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double tmp122 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp123 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
						double lam1 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp114+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]))),(tmp115+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])))}),0.0})))+std::max({std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp120)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp122))),std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp121)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp123)))}));
						double tmp124 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp106)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp108));
						double tmp125 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp107)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp109));
						double tmp126 = ((((tmp106*tmp110)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp104)+(0.08*tmp104*tmp104))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp106*tmp112)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp104));
						double tmp127 = ((((tmp107*tmp111)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp105)+(0.08*tmp105*tmp105))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp107*tmp113)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp105));
						double tmp128 = ((((tmp108*tmp112)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp104)+(0.08*tmp104*tmp104))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp108*tmp110)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp104));
						double tmp129 = ((((tmp109*tmp113)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp105)+(0.08*tmp105*tmp105))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp109*tmp111)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp105));
						fieldData_cetaEdgeVUpper0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(((tmp105-tmp104)*lam1)-(tmp124+tmp125))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
						fieldData_cuEdgeVUpper0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(((tmp107-tmp106)*lam1)-(tmp126+tmp127))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
						fieldData_cvEdgeVUpper0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(((tmp109-tmp108)*lam1)-(tmp128+tmp129))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					}
				}
			}
		} else if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 8; i0<9; i0 += 1) {
					double tmp130 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp131 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp132 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp133 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp134 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp135 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp136 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp137 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp138 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp139 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp140 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp141 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp142 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp143 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp144 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp145 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp146 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp147 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double tmp148 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp149 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+10)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+10)]);
					double lam1 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp140+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]))),(tmp141+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])))}),0.0})))+std::max({std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp146)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp148))),std::fabs(((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp147)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp149)))}));
					double tmp150 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp132)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp134));
					double tmp151 = ((fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp133)+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp135));
					double tmp152 = ((((tmp132*tmp136)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp130)+(0.08*tmp130*tmp130))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp132*tmp138)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp130));
					double tmp153 = ((((tmp133*tmp137)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp131)+(0.08*tmp131*tmp131))*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp133*tmp139)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp131));
					double tmp154 = ((((tmp134*tmp138)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp130)+(0.08*tmp130*tmp130))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp134*tmp136)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp130));
					double tmp155 = ((((tmp135*tmp139)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+23)]*tmp131)+(0.08*tmp131*tmp131))*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)])+(fieldData_normalVertical0[fragmentIdx][((11*i1)+i0+12)]*tmp135*tmp137)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]-fieldData_bath[fragmentIdx][((11*i1)+i0+23)])*fieldData_normalVertical1[fragmentIdx][((11*i1)+i0+12)]*tmp131));
					fieldData_cetaEdgeVUpper0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(((tmp131-tmp130)*lam1)-(tmp150+tmp151))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_cuEdgeVUpper0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(((tmp133-tmp132)*lam1)-(tmp152+tmp153))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
					fieldData_cvEdgeVUpper0[fragmentIdx][((11*i1)+i0+12)] = (0.70710678118655*(((tmp135-tmp134)*lam1)-(tmp154+tmp155))*fieldData_edgeLenVertical0[fragmentIdx][((11*i1)+i0+12)]);
				}
			}
		}
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 1; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					double tmp156 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp157 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp158 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp159 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp160 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp161 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp162 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp163 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp164 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp165 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp166 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp167 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp168 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp169 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp170 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp171 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp172 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp173 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp174 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp175 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double lam2 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp166+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]))),(tmp167+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)])))}),0.0})))+std::max({std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp172)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp174))),std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp173)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp175)))}));
					double tmp176 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp158)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp160));
					double tmp177 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp159)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp161));
					double tmp178 = ((((tmp158*tmp162)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp156)+(0.08*tmp156*tmp156))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp158*tmp164)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp156));
					double tmp179 = ((((tmp159*tmp163)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp157)+(0.08*tmp157*tmp157))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp159*tmp165)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp157));
					double tmp180 = ((((tmp160*tmp164)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp156)+(0.08*tmp156*tmp156))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp160*tmp162)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp156));
					double tmp181 = ((((tmp161*tmp165)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp157)+(0.08*tmp157*tmp157))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp161*tmp163)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp157));
					fieldData_cetaEdgeHLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp176+tmp177+((tmp156-tmp157)*lam2))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cuEdgeHLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp178+tmp179+((tmp158-tmp159)*lam2))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvEdgeHLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp180+tmp181+((tmp160-tmp161)*lam2))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cetaEdgeHUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp157-tmp156)*lam2)-(tmp176+tmp177))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cuEdgeHUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp159-tmp158)*lam2)-(tmp178+tmp179))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvEdgeHUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp161-tmp160)*lam2)-(tmp180+tmp181))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
		if ((((commNeighIdx[2][fragmentIdx]<0)||(commNeighIdx[2][fragmentIdx]==2))||(commNeighIdx[2][fragmentIdx]==3))) {
			if (isValidForDomain[fragmentIdx]) {
				for (int i1 = 0; i1<1; i1 += 1) {
					for (int i0 = 0; i0<8; i0 += 1) {
						double tmp182 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp183 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp184 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp185 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp186 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp187 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp188 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp189 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp190 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp191 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp192 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp193 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp194 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp195 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp196 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp197 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp198 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp199 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp200 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp201 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double lam2 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp192+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]))),(tmp193+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)])))}),0.0})))+std::max({std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp198)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp200))),std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp199)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp201)))}));
						double tmp202 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp184)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp186));
						double tmp203 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp185)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp187));
						double tmp204 = ((((tmp184*tmp188)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp182)+(0.08*tmp182*tmp182))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp184*tmp190)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp182));
						double tmp205 = ((((tmp185*tmp189)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp183)+(0.08*tmp183*tmp183))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp185*tmp191)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp183));
						double tmp206 = ((((tmp186*tmp190)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp182)+(0.08*tmp182*tmp182))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp186*tmp188)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp182));
						double tmp207 = ((((tmp187*tmp191)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp183)+(0.08*tmp183*tmp183))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp187*tmp189)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp183));
						fieldData_cetaEdgeHLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp202+tmp203+((tmp182-tmp183)*lam2))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
						fieldData_cuEdgeHLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp204+tmp205+((tmp184-tmp185)*lam2))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
						fieldData_cvEdgeHLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp206+tmp207+((tmp186-tmp187)*lam2))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					}
				}
			}
		} else if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<1; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					double tmp208 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp209 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp210 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp211 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp212 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp213 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp214 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp215 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp216 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp217 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp218 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp219 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp220 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp221 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp222 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp223 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp224 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp225 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp226 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp227 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double lam2 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp218+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]))),(tmp219+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)])))}),0.0})))+std::max({std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp224)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp226))),std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp225)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp227)))}));
					double tmp228 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp210)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp212));
					double tmp229 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp211)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp213));
					double tmp230 = ((((tmp210*tmp214)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp208)+(0.08*tmp208*tmp208))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp210*tmp216)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp208));
					double tmp231 = ((((tmp211*tmp215)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp209)+(0.08*tmp209*tmp209))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp211*tmp217)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp209));
					double tmp232 = ((((tmp212*tmp216)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp208)+(0.08*tmp208*tmp208))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp212*tmp214)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp208));
					double tmp233 = ((((tmp213*tmp217)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp209)+(0.08*tmp209*tmp209))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp213*tmp215)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp209));
					fieldData_cetaEdgeHLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp228+tmp229+((tmp208-tmp209)*lam2))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cuEdgeHLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp230+tmp231+((tmp210-tmp211)*lam2))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvEdgeHLower0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(tmp232+tmp233+((tmp212-tmp213)*lam2))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
		if ((((commNeighIdx[3][fragmentIdx]<0)||(commNeighIdx[3][fragmentIdx]==2))||(commNeighIdx[3][fragmentIdx]==3))) {
			if (isValidForDomain[fragmentIdx]) {
				for (int i1 = 8; i1<9; i1 += 1) {
					for (int i0 = 0; i0<8; i0 += 1) {
						double tmp234 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp235 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp236 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp237 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp238 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp239 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp240 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp241 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp242 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp243 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp244 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp245 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp246 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp247 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp248 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp249 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp250 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp251 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double tmp252 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
						double tmp253 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
						double lam2 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp244+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]))),(tmp245+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)])))}),0.0})))+std::max({std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp250)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp252))),std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp251)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp253)))}));
						double tmp254 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp236)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp238));
						double tmp255 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp237)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp239));
						double tmp256 = ((((tmp236*tmp240)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp234)+(0.08*tmp234*tmp234))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp236*tmp242)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp234));
						double tmp257 = ((((tmp237*tmp241)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp235)+(0.08*tmp235*tmp235))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp237*tmp243)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp235));
						double tmp258 = ((((tmp238*tmp242)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp234)+(0.08*tmp234*tmp234))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp238*tmp240)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp234));
						double tmp259 = ((((tmp239*tmp243)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp235)+(0.08*tmp235*tmp235))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp239*tmp241)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp235));
						fieldData_cetaEdgeHUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp235-tmp234)*lam2)-(tmp254+tmp255))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
						fieldData_cuEdgeHUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp237-tmp236)*lam2)-(tmp256+tmp257))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
						fieldData_cvEdgeHUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp239-tmp238)*lam2)-(tmp258+tmp259))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					}
				}
			}
		} else if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 8; i1<9; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					double tmp260 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp261 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp262 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp263 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp264 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp265 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp266 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp267 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp268 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp269 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp270 = (1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp271 = (1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp272 = (1.4142135623731*fieldData_cuLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp273 = (1.4142135623731*fieldData_cuUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp274 = (1.4142135623731*fieldData_cvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp275 = (1.4142135623731*fieldData_cvUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp276 = (1.4142135623731*fieldData_cuTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp277 = (1.4142135623731*fieldData_cuTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double tmp278 = (1.4142135623731*fieldData_cvTildeLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]);
					double tmp279 = (1.4142135623731*fieldData_cvTildeUpper0[fragmentIdx][((10*i1)+i0+1)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+1)]);
					double lam2 = ((std::sqrt(0.16)*std::sqrt(std::max({std::max({(tmp270+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]))),(tmp271+(0.5*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)])))}),0.0})))+std::max({std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp276)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp278))),std::fabs(((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp277)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp279)))}));
					double tmp280 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp262)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp264));
					double tmp281 = ((fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp263)+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp265));
					double tmp282 = ((((tmp262*tmp266)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp260)+(0.08*tmp260*tmp260))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp262*tmp268)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp260));
					double tmp283 = ((((tmp263*tmp267)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp261)+(0.08*tmp261*tmp261))*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp263*tmp269)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp261));
					double tmp284 = ((((tmp264*tmp268)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp260)+(0.08*tmp260*tmp260))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp264*tmp266)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp260));
					double tmp285 = ((((tmp265*tmp269)+(0.16*fieldData_bath[fragmentIdx][((11*i1)+i0+12)]*tmp261)+(0.08*tmp261*tmp261))*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)])+(fieldData_normalHorizontal0[fragmentIdx][((10*i1)+i0+11)]*tmp265*tmp267)+(0.08*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_normalHorizontal1[fragmentIdx][((10*i1)+i0+11)]*tmp261));
					fieldData_cetaEdgeHUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp261-tmp260)*lam2)-(tmp280+tmp281))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cuEdgeHUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp263-tmp262)*lam2)-(tmp282+tmp283))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
					fieldData_cvEdgeHUpper0[fragmentIdx][((10*i1)+i0+11)] = (0.70710678118655*(((tmp265-tmp264)*lam2)-(tmp284+tmp285))*fieldData_edgeLenHorizontal0[fragmentIdx][((10*i1)+i0+11)]);
				}
			}
		}
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldData_cetaNewLower0[fragmentIdx][((10*i1)+i0+11)] = (-((fieldData_cetaEdgeDLower0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cetaEdgeHLower0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cetaEdgeVLower0[fragmentIdx][((11*i1)+i0+12)])*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]));
					fieldData_cuNewLower0[fragmentIdx][((10*i1)+i0+11)] = ((0.70710678118655*((0.22627416997969602*(((fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_bLower3[fragmentIdx][((10*i1)+i0+11)])-((fieldData_bath[fragmentIdx][((11*i1)+i0+23)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_bLower2[fragmentIdx][((10*i1)+i0+11)]))*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_detBInvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.16*(((fieldData_tidal[fragmentIdx][((11*i1)+i0+13)]-fieldData_tidal[fragmentIdx][((11*i1)+i0+12)])*fieldData_bLower3[fragmentIdx][((10*i1)+i0+11)])-((fieldData_tidal[fragmentIdx][((11*i1)+i0+23)]-fieldData_tidal[fragmentIdx][((11*i1)+i0+12)])*fieldData_bLower2[fragmentIdx][((10*i1)+i0+11)]))*((1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.333333333333333*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])))*fieldData_detBInvLower0[fragmentIdx][((10*i1)+i0+11)]))*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)])-((fieldData_cuEdgeDLower0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cuEdgeHLower0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cuEdgeVLower0[fragmentIdx][((11*i1)+i0+12)])*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]));
					fieldData_cvNewLower0[fragmentIdx][((10*i1)+i0+11)] = ((0.70710678118655*((0.22627416997969602*(((fieldData_bath[fragmentIdx][((11*i1)+i0+23)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_bLower0[fragmentIdx][((10*i1)+i0+11)])-((fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+12)])*fieldData_bLower1[fragmentIdx][((10*i1)+i0+11)]))*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_detBInvLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.16*(((fieldData_tidal[fragmentIdx][((11*i1)+i0+23)]-fieldData_tidal[fragmentIdx][((11*i1)+i0+12)])*fieldData_bLower0[fragmentIdx][((10*i1)+i0+11)])-((fieldData_tidal[fragmentIdx][((11*i1)+i0+13)]-fieldData_tidal[fragmentIdx][((11*i1)+i0+12)])*fieldData_bLower1[fragmentIdx][((10*i1)+i0+11)]))*((1.4142135623731*fieldData_cetaLower0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)])+(0.333333333333333*(fieldData_bath[fragmentIdx][((11*i1)+i0+12)]+fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)])))*fieldData_detBInvLower0[fragmentIdx][((10*i1)+i0+11)]))*fieldData_sqrtDetBLower0[fragmentIdx][((10*i1)+i0+11)])-((fieldData_cvEdgeDLower0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cvEdgeHLower0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cvEdgeVLower0[fragmentIdx][((11*i1)+i0+12)])*fieldData_sqrtInvDetBLower0[fragmentIdx][((10*i1)+i0+11)]));
				}
			}
		}
		if (isValidForDomain[fragmentIdx]) {
			for (int i1 = 0; i1<8; i1 += 1) {
				for (int i0 = 0; i0<8; i0 += 1) {
					fieldData_cetaNewUpper0[fragmentIdx][((10*i1)+i0+11)] = (-((fieldData_cetaEdgeDUpper0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cetaEdgeHUpper0[fragmentIdx][((10*i1)+i0+21)]+fieldData_cetaEdgeVUpper0[fragmentIdx][((11*i1)+i0+13)])*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]));
					fieldData_cuNewUpper0[fragmentIdx][((10*i1)+i0+11)] = ((0.70710678118655*((0.22627416997969602*(((fieldData_bath[fragmentIdx][((11*i1)+i0+23)]-fieldData_bath[fragmentIdx][((11*i1)+i0+24)])*fieldData_bUpper3[fragmentIdx][((10*i1)+i0+11)])-((fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+24)])*fieldData_bUpper2[fragmentIdx][((10*i1)+i0+11)]))*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_detBInvUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.16*(((fieldData_tidal[fragmentIdx][((11*i1)+i0+23)]-fieldData_tidal[fragmentIdx][((11*i1)+i0+24)])*fieldData_bUpper3[fragmentIdx][((10*i1)+i0+11)])-((fieldData_tidal[fragmentIdx][((11*i1)+i0+13)]-fieldData_tidal[fragmentIdx][((11*i1)+i0+24)])*fieldData_bUpper2[fragmentIdx][((10*i1)+i0+11)]))*((1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.333333333333333*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)])))*fieldData_detBInvUpper0[fragmentIdx][((10*i1)+i0+11)]))*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)])-((fieldData_cuEdgeDUpper0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cuEdgeHUpper0[fragmentIdx][((10*i1)+i0+21)]+fieldData_cuEdgeVUpper0[fragmentIdx][((11*i1)+i0+13)])*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]));
					fieldData_cvNewUpper0[fragmentIdx][((10*i1)+i0+11)] = ((0.70710678118655*((0.22627416997969602*(((fieldData_bath[fragmentIdx][((11*i1)+i0+13)]-fieldData_bath[fragmentIdx][((11*i1)+i0+24)])*fieldData_bUpper0[fragmentIdx][((10*i1)+i0+11)])-((fieldData_bath[fragmentIdx][((11*i1)+i0+23)]-fieldData_bath[fragmentIdx][((11*i1)+i0+24)])*fieldData_bUpper1[fragmentIdx][((10*i1)+i0+11)]))*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_detBInvUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.16*(((fieldData_tidal[fragmentIdx][((11*i1)+i0+13)]-fieldData_tidal[fragmentIdx][((11*i1)+i0+24)])*fieldData_bUpper0[fragmentIdx][((10*i1)+i0+11)])-((fieldData_tidal[fragmentIdx][((11*i1)+i0+23)]-fieldData_tidal[fragmentIdx][((11*i1)+i0+24)])*fieldData_bUpper1[fragmentIdx][((10*i1)+i0+11)]))*((1.4142135623731*fieldData_cetaUpper0[fragmentIdx][((10*i1)+i0+11)]*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)])+(0.333333333333333*(fieldData_bath[fragmentIdx][((11*i1)+i0+13)]+fieldData_bath[fragmentIdx][((11*i1)+i0+23)]+fieldData_bath[fragmentIdx][((11*i1)+i0+24)])))*fieldData_detBInvUpper0[fragmentIdx][((10*i1)+i0+11)]))*fieldData_sqrtDetBUpper0[fragmentIdx][((10*i1)+i0+11)])-((fieldData_cvEdgeDUpper0[fragmentIdx][((10*i1)+i0+11)]+fieldData_cvEdgeHUpper0[fragmentIdx][((10*i1)+i0+21)]+fieldData_cvEdgeVUpper0[fragmentIdx][((11*i1)+i0+13)])*fieldData_sqrtInvDetBUpper0[fragmentIdx][((10*i1)+i0+11)]));
				}
			}
		}
	}
	/* -.-.-.- inlined stopTimer -.-.-.- */
	StopWatch& stopWatch = timer_UpdateUnknowns;
	(--stopWatch.numEntries);
	if ((0==stopWatch.numEntries)) {
		
		stopWatch.timerEnded = std::chrono::high_resolution_clock::now();
		stopWatch.lastTimeMeasured = std::chrono::duration_cast<std::chrono::nanoseconds>((stopWatch.timerEnded-stopWatch.timerStarted));
		stopWatch.totalTimeMeasured += stopWatch.lastTimeMeasured;
		(++stopWatch.numMeasurements);
	}
	/* =^=^=^=^= end stopTimer =^=^=^=^= */
}
