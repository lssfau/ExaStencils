#include "fstream"
#include "sstream"
#include "iomanip"

#include "Communication/Communication.h"

void exchbath_all_a_a_a_a_3 (int slot) {
	for (int fragmentIdx = 0; fragmentIdx<28; ++fragmentIdx) {
		if ((fragmentId[fragmentIdx]>neighFragId[0][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]>=neighFragId[1][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]>neighFragId[2][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]>=neighFragId[3][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx])) {
			
		}
		if (((((fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[0][fragmentIdx]) {
					;
					switch (commTrafoId[0][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((11*i1)+i0+8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(((11*i0)+98)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(112-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((i1+22)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[1][fragmentIdx]) {
					;
					switch (commTrafoId[1][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(((11*i1)+i0)-8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((11*i0)-(i1+78))];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(128-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((i1+198)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[2][fragmentIdx]) {
					;
					switch (commTrafoId[2][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((11*i1)+i0+88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(((11*i0)+2)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(32-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((i1+118)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[3][fragmentIdx]) {
					;
					switch (commTrafoId[3][fragmentIdx]) {
						case 0: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i1)+i0)-88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i0)+18)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(208-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][((i1+102)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[1][fragmentIdx]) {
					;
					switch (commTrafoId[1][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(((11*i1)+i0)-8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((11*i0)-(i1+78))];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(128-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((i1+198)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[2][fragmentIdx]) {
					;
					switch (commTrafoId[2][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((11*i1)+i0+88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(((11*i0)+2)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(32-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((i1+118)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[3][fragmentIdx]) {
					;
					switch (commTrafoId[3][fragmentIdx]) {
						case 0: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i1)+i0)-88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i0)+18)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(208-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][((i1+102)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[2][fragmentIdx]) {
					;
					switch (commTrafoId[2][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((11*i1)+i0+88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(((11*i0)+2)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(32-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((i1+118)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[3][fragmentIdx]) {
					;
					switch (commTrafoId[3][fragmentIdx]) {
						case 0: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i1)+i0)-88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i0)+18)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(208-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][((i1+102)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[3][fragmentIdx]) {
					;
					switch (commTrafoId[3][fragmentIdx]) {
						case 0: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i1)+i0)-88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i0)+18)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(208-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][((i1+102)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[2][fragmentIdx]) {
					;
					switch (commTrafoId[2][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((11*i1)+i0+88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(((11*i0)+2)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(32-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((i1+118)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[1][fragmentIdx]) {
					;
					switch (commTrafoId[1][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(((11*i1)+i0)-8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((11*i0)-(i1+78))];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(128-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((i1+198)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[3][fragmentIdx]) {
					;
					switch (commTrafoId[3][fragmentIdx]) {
						case 0: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i1)+i0)-88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i0)+18)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(208-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][((i1+102)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[1][fragmentIdx]) {
					;
					switch (commTrafoId[1][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(((11*i1)+i0)-8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((11*i0)-(i1+78))];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(128-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((i1+198)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[1][fragmentIdx]) {
					;
					switch (commTrafoId[1][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(((11*i1)+i0)-8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((11*i0)-(i1+78))];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(128-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((i1+198)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[2][fragmentIdx]) {
					;
					switch (commTrafoId[2][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((11*i1)+i0+88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(((11*i0)+2)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(32-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((i1+118)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx])||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[1][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[0][fragmentIdx]) {
					;
					switch (commTrafoId[0][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((11*i1)+i0+8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(((11*i0)+98)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(112-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((i1+22)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[2][fragmentIdx]) {
					;
					switch (commTrafoId[2][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((11*i1)+i0+88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(((11*i0)+2)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(32-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((i1+118)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[3][fragmentIdx]) {
					;
					switch (commTrafoId[3][fragmentIdx]) {
						case 0: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i1)+i0)-88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i0)+18)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(208-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][((i1+102)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx])||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[1][fragmentIdx]) {
				
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[0][fragmentIdx]) {
					;
					switch (commTrafoId[0][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((11*i1)+i0+8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(((11*i0)+98)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(112-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((i1+22)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[3][fragmentIdx]) {
					;
					switch (commTrafoId[3][fragmentIdx]) {
						case 0: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i1)+i0)-88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i0)+18)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(208-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][((i1+102)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx])||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))) {
			if (neighbor_isValid[1][fragmentIdx]) {
				
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[0][fragmentIdx]) {
					;
					switch (commTrafoId[0][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((11*i1)+i0+8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(((11*i0)+98)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(112-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((i1+22)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))) {
			if (neighbor_isValid[1][fragmentIdx]) {
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[0][fragmentIdx]) {
					;
					switch (commTrafoId[0][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((11*i1)+i0+8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(((11*i0)+98)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(112-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((i1+22)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[2][fragmentIdx]) {
					;
					switch (commTrafoId[2][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((11*i1)+i0+88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(((11*i0)+2)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(32-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((i1+118)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx])||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[2][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[0][fragmentIdx]) {
					;
					switch (commTrafoId[0][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((11*i1)+i0+8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(((11*i0)+98)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(112-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((i1+22)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[1][fragmentIdx]) {
					;
					switch (commTrafoId[1][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(((11*i1)+i0)-8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((11*i0)-(i1+78))];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(128-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((i1+198)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[3][fragmentIdx]) {
					;
					switch (commTrafoId[3][fragmentIdx]) {
						case 0: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i1)+i0)-88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i0)+18)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(208-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 9; i1<10; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][((i1+102)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))) {
			if (neighbor_isValid[2][fragmentIdx]) {
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[0][fragmentIdx]) {
					;
					switch (commTrafoId[0][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((11*i1)+i0+8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(((11*i0)+98)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(112-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((i1+22)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[1][fragmentIdx]) {
					;
					switch (commTrafoId[1][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(((11*i1)+i0)-8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((11*i0)-(i1+78))];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(128-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((i1+198)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if (((((fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx])||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))) {
			if (neighbor_isValid[3][fragmentIdx]) {
				
			}
			if (isValidForDomain[fragmentIdx]) {
				if (neighbor_isValid[0][fragmentIdx]) {
					;
					switch (commTrafoId[0][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((11*i1)+i0+8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(((11*i0)+98)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(112-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 1; i0<2; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((i1+22)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[1][fragmentIdx]) {
					;
					switch (commTrafoId[1][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(((11*i1)+i0)-8)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((11*i0)-(i1+78))];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(128-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<10; i1 += 1) {
								for (int i0 = 9; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((i1+198)-(11*i0))];
								}
							}
						} break;
					}
					
				}
				if (neighbor_isValid[2][fragmentIdx]) {
					;
					switch (commTrafoId[2][fragmentIdx]) {
						case 0: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((11*i1)+i0+88)];
								}
							}
						} break;
						case 1: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(((11*i0)+2)-i1)];
								}
							}
						} break;
						case 2: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(32-((11*i1)+i0))];
								}
							}
						} break;
						case 3: {
							for (int i1 = 1; i1<2; i1 += 1) {
								for (int i0 = 1; i0<10; i0 += 1) {
									fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((i1+118)-(11*i0))];
								}
							}
						} break;
					}
					
				}
			}
		}
		if ((fragmentId[fragmentIdx]<=neighFragId[0][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]<neighFragId[1][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]<=neighFragId[2][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]<neighFragId[3][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]>neighFragId[0][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]>=neighFragId[1][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]>neighFragId[2][fragmentIdx])) {
			
		}
		if ((fragmentId[fragmentIdx]>=neighFragId[3][fragmentIdx])) {
			
		}
		if (((((fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))) {
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
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[0][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))) {
			if (neighbor_isValid[0][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx])||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[1][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx])||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[1][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx])||(fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx]))||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))) {
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
		if (((((fragmentId[fragmentIdx]<=neighFragId[1][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))) {
			if (neighbor_isValid[1][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx])||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[3][fragmentIdx]))) {
			if (neighbor_isValid[2][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<neighFragId[2][fragmentIdx])||(fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))) {
			if (neighbor_isValid[2][fragmentIdx]) {
				;
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				;
			}
		}
		if (((((fragmentId[fragmentIdx]<=neighFragId[3][fragmentIdx])||(fragmentId[fragmentIdx]>=neighFragId[0][fragmentIdx]))||(fragmentId[fragmentIdx]>neighFragId[1][fragmentIdx]))||(fragmentId[fragmentIdx]>=neighFragId[2][fragmentIdx]))) {
			if (neighbor_isValid[3][fragmentIdx]) {
				;
			}
		}
	}
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
						for (int i1 = 1; i1<10; i1 += 1) {
							for (int i0 = 0; i0<1; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((11*i1)+i0+8)];
							}
						}
					} break;
					case 1: {
						for (int i1 = 1; i1<10; i1 += 1) {
							for (int i0 = 0; i0<1; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(((11*i0)+98)-i1)];
							}
						}
					} break;
					case 2: {
						for (int i1 = 1; i1<10; i1 += 1) {
							for (int i0 = 0; i0<1; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][(112-((11*i1)+i0))];
							}
						}
					} break;
					case 3: {
						for (int i1 = 1; i1<10; i1 += 1) {
							for (int i0 = 0; i0<1; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[0][fragmentIdx]][((i1+22)-(11*i0))];
							}
						}
					} break;
				}
				
			}
			if (neighbor_isValid[1][fragmentIdx]) {
				;
				switch (commTrafoId[1][fragmentIdx]) {
					case 0: {
						for (int i1 = 1; i1<10; i1 += 1) {
							for (int i0 = 10; i0<11; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(((11*i1)+i0)-8)];
							}
						}
					} break;
					case 1: {
						for (int i1 = 1; i1<10; i1 += 1) {
							for (int i0 = 10; i0<11; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((11*i0)-(i1+78))];
							}
						}
					} break;
					case 2: {
						for (int i1 = 1; i1<10; i1 += 1) {
							for (int i0 = 10; i0<11; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][(128-((11*i1)+i0))];
							}
						}
					} break;
					case 3: {
						for (int i1 = 1; i1<10; i1 += 1) {
							for (int i0 = 10; i0<11; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[1][fragmentIdx]][((i1+198)-(11*i0))];
							}
						}
					} break;
				}
				
			}
			if (neighbor_isValid[2][fragmentIdx]) {
				;
				switch (commTrafoId[2][fragmentIdx]) {
					case 0: {
						for (int i1 = 0; i1<1; i1 += 1) {
							for (int i0 = 1; i0<10; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((11*i1)+i0+88)];
							}
						}
					} break;
					case 1: {
						for (int i1 = 0; i1<1; i1 += 1) {
							for (int i0 = 1; i0<10; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(((11*i0)+2)-i1)];
							}
						}
					} break;
					case 2: {
						for (int i1 = 0; i1<1; i1 += 1) {
							for (int i0 = 1; i0<10; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][(32-((11*i1)+i0))];
							}
						}
					} break;
					case 3: {
						for (int i1 = 0; i1<1; i1 += 1) {
							for (int i0 = 1; i0<10; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[2][fragmentIdx]][((i1+118)-(11*i0))];
							}
						}
					} break;
				}
				
			}
			if (neighbor_isValid[3][fragmentIdx]) {
				;
				switch (commTrafoId[3][fragmentIdx]) {
					case 0: {
						for (int i1 = 10; i1<11; i1 += 1) {
							for (int i0 = 1; i0<10; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i1)+i0)-88)];
							}
						}
					} break;
					case 1: {
						for (int i1 = 10; i1<11; i1 += 1) {
							for (int i0 = 1; i0<10; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(((11*i0)+18)-i1)];
							}
						}
					} break;
					case 2: {
						for (int i1 = 10; i1<11; i1 += 1) {
							for (int i0 = 1; i0<10; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][(208-((11*i1)+i0))];
							}
						}
					} break;
					case 3: {
						for (int i1 = 10; i1<11; i1 += 1) {
							for (int i0 = 1; i0<10; i0 += 1) {
								fieldData_bath[fragmentIdx][((11*i1)+i0)] = fieldData_bath[neighbor_fragCommId[3][fragmentIdx]][((i1+102)-(11*i0))];
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
