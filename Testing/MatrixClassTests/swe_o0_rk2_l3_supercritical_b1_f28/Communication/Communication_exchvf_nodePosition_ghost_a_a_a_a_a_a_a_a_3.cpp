#include "fstream"
#include "sstream"
#include "iomanip"

#include "Communication/Communication.h"

void exchvf_nodePosition_ghost_a_a_a_a_a_a_a_a_3 (int slot) {
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (neighbor_isValid[0][fragmentIdx]) {
			
		}
		if (neighbor_isValid[1][fragmentIdx]) {
			
		}
		if (neighbor_isValid[2][fragmentIdx]) {
			
		}
		if (neighbor_isValid[3][fragmentIdx]) {
			
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (isValidForDomain[fragmentIdx]) {
			if (neighbor_isValid[0][fragmentIdx]) {
				;
				switch (commTrafoId[0][fragmentIdx]) {
					case 0: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 2; i1<11; i1 += 1) {
									for (int i0 = 0; i0<2; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[0][fragmentIdx]][((338*i3)+(169*i2)+(13*i1)+i0+8)];
									}
								}
							}
						}
					} break;
					case 1: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 2; i1<11; i1 += 1) {
									for (int i0 = 0; i0<2; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[0][fragmentIdx]][(((338*i3)+(169*i2)+(13*i0)+116)-i1)];
									}
								}
							}
						}
					} break;
					case 2: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 2; i1<11; i1 += 1) {
									for (int i0 = 0; i0<2; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[0][fragmentIdx]][(((338*i3)+(169*i2)+160)-((13*i1)+i0))];
									}
								}
							}
						}
					} break;
					case 3: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 2; i1<11; i1 += 1) {
									for (int i0 = 0; i0<2; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[0][fragmentIdx]][(((338*i3)+(169*i2)+i1+52)-(13*i0))];
									}
								}
							}
						}
					} break;
				}
				
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				;
				switch (commTrafoId[1][fragmentIdx]) {
					case 0: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 2; i1<11; i1 += 1) {
									for (int i0 = 11; i0<13; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[1][fragmentIdx]][(((338*i3)+(169*i2)+(13*i1)+i0)-8)];
									}
								}
							}
						}
					} break;
					case 1: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 2; i1<11; i1 += 1) {
									for (int i0 = 11; i0<13; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[1][fragmentIdx]][(((338*i3)+(169*i2)+(13*i0))-(i1+92))];
									}
								}
							}
						}
					} break;
					case 2: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 2; i1<11; i1 += 1) {
									for (int i0 = 11; i0<13; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[1][fragmentIdx]][(((338*i3)+(169*i2)+176)-((13*i1)+i0))];
									}
								}
							}
						}
					} break;
					case 3: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 2; i1<11; i1 += 1) {
									for (int i0 = 11; i0<13; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[1][fragmentIdx]][(((338*i3)+(169*i2)+i1+260)-(13*i0))];
									}
								}
							}
						}
					} break;
				}
				
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				;
				switch (commTrafoId[2][fragmentIdx]) {
					case 0: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 0; i1<2; i1 += 1) {
									for (int i0 = 2; i0<11; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[2][fragmentIdx]][((338*i3)+(169*i2)+(13*i1)+i0+104)];
									}
								}
							}
						}
					} break;
					case 1: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 0; i1<2; i1 += 1) {
									for (int i0 = 2; i0<11; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[2][fragmentIdx]][(((338*i3)+(169*i2)+(13*i0)+4)-i1)];
									}
								}
							}
						}
					} break;
					case 2: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 0; i1<2; i1 += 1) {
									for (int i0 = 2; i0<11; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[2][fragmentIdx]][(((338*i3)+(169*i2)+64)-((13*i1)+i0))];
									}
								}
							}
						}
					} break;
					case 3: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 0; i1<2; i1 += 1) {
									for (int i0 = 2; i0<11; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[2][fragmentIdx]][(((338*i3)+(169*i2)+i1+164)-(13*i0))];
									}
								}
							}
						}
					} break;
				}
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				;
				switch (commTrafoId[3][fragmentIdx]) {
					case 0: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 11; i1<13; i1 += 1) {
									for (int i0 = 2; i0<11; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[3][fragmentIdx]][(((338*i3)+(169*i2)+(13*i1)+i0)-104)];
									}
								}
							}
						}
					} break;
					case 1: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 11; i1<13; i1 += 1) {
									for (int i0 = 2; i0<11; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[3][fragmentIdx]][(((338*i3)+(169*i2)+(13*i0)+20)-i1)];
									}
								}
							}
						}
					} break;
					case 2: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 11; i1<13; i1 += 1) {
									for (int i0 = 2; i0<11; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[3][fragmentIdx]][(((338*i3)+(169*i2)+272)-((13*i1)+i0))];
									}
								}
							}
						}
					} break;
					case 3: {
						for (int i3 = 0; i3<1; i3 += 1) {
							for (int i2 = 0; i2<2; i2 += 1) {
								for (int i1 = 11; i1<13; i1 += 1) {
									for (int i0 = 2; i0<11; i0 += 1) {
										fieldData_vf_nodePosition[fragmentIdx][((338*i3)+(169*i2)+(13*i1)+i0)] = fieldData_vf_nodePosition[neighbor_fragCommId[3][fragmentIdx]][(((338*i3)+(169*i2)+i1+148)-(13*i0))];
									}
								}
							}
						}
					} break;
				}
				
			}
		}
	}
	#pragma omp parallel for schedule(static) num_threads(8)
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if (neighbor_isValid[0][fragmentIdx]) {
			;
		}
		if (neighbor_isValid[1][fragmentIdx]) {
			;
		}
		if (neighbor_isValid[2][fragmentIdx]) {
			;
		}
		if (neighbor_isValid[3][fragmentIdx]) {
			;
		}
	}
}
