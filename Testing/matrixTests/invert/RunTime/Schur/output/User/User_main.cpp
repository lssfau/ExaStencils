#include "User/User.h"

int main (int argc, char** argv) {
	__matrix_double_7_7_t mat {1.0, 2.0, 1.0, 0.0, 0.0, 0.0, 3.0, 4.0, 2.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 5.0, 1.0, 0.0, 0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 4.0, 1.0, 3.0, 2.0, 0.0, 0.0, 0.0, 1.0, 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 4.0, 6.0, 7.0, 3.0, 3.0, 5.0, 7.0, 4.0, 4.0, 5.0, 1.0};
	__matrix_double_7_7_t mat_inverse;
	{
		int offset_r;
		int offset_c;
		offset_r = 0;
		offset_c = 0;
		int n;
		n = 3;
		int m;
		m = 4;
		__matrix_double_3_3_t A;
		{
			int i;
			int j;
			for (i = offset_r; i<(n+offset_r); ++i) {
				for (j = offset_c; j<(offset_c+n); ++j) {
					A[(((3*(i-offset_r))+j)-offset_c)] = mat[((7*i)+j)];
				}
			}
		}
		__matrix_double_3_3_t A_inv;
		{
			int P[4];
			int i;
			int j;
			int k;
			double tmp_row[3];
			int imax;
			double maxA;
			double absA;
			for (i = 0; i<4; ++i) {
				P[i] = i;
			}
			for (i = 0; i<3; ++i) {
				maxA = 0.0;
				imax = i;
				for (k = i; k<3; ++k) {
					absA = fabs(A[((3*(k+offset_r))+i+offset_c)]);
					if ((absA>maxA)) {
						maxA = absA;
						imax = k;
					}
				}
				if ((maxA<1.0E-4)) {
					printf("[WARNING] Inverting potentially singular matrix\n");
					return -1;
				}
				if ((imax!=i)) {
					j = P[i];
					P[i] = P[imax];
					P[imax] = j;
					std::memcpy((&(tmp_row[0])), (&(A[((3*(i+offset_r))+offset_c)])), (3*sizeof(double)));
					std::memcpy((&(A[((3*(i+offset_r))+offset_c)])), (&(A[((3*(imax+offset_r))+offset_c)])), (3*sizeof(double)));
					std::memcpy((&(A[((3*(imax+offset_r))+offset_c)])), (&(tmp_row[0])), (3*sizeof(double)));
					(P[4]++);
				}
				for (j = (i+1); j<3; ++j) {
					A[((3*(j+offset_r))+i+offset_c)] = (A[((3*(j+offset_r))+i+offset_c)]/A[((4*i)+(3*offset_r)+offset_c)]);
					for (k = (i+1); k<3; ++k) {
						A[((3*(j+offset_r))+k+offset_c)] = (A[((3*(j+offset_r))+k+offset_c)]-(A[((3*(j+offset_r))+i+offset_c)]*A[((3*(i+offset_r))+k+offset_c)]));
					}
				}
			}
			for (j = 0; j<3; ++j) {
				for (i = 0; i<3; ++i) {
					if ((P[i]==j)) {
						A_inv[((3*(i+offset_r))+j+offset_c)] = 1.0;
					} else {
						A_inv[((3*(i+offset_r))+j+offset_c)] = 0.0;
					}
					for (k = 0; k<i; ++k) {
						A_inv[((3*(i+offset_r))+j+offset_c)] = (A_inv[((3*(i+offset_r))+j+offset_c)]-(A[((3*(i+offset_r))+k+offset_c)]*A_inv[((3*(k+offset_r))+j+offset_c)]));
					}
				}
				for (i = 2; i>=0; i--) {
					for (k = (i+1); k<3; ++k) {
						A_inv[((3*(i+offset_r))+j+offset_c)] = (A_inv[((3*(i+offset_r))+j+offset_c)]-(A[((3*(i+offset_r))+k+offset_c)]*A_inv[((3*(k+offset_r))+j+offset_c)]));
					}
					A_inv[((3*(i+offset_r))+j+offset_c)] = (A_inv[((3*(i+offset_r))+j+offset_c)]/A[((4*i)+(3*offset_r)+offset_c)]);
				}
			}
		}
		__matrix_double_3_4_t B;
		{
			int i;
			int j;
			for (i = offset_r; i<(n+offset_r); ++i) {
				for (j = n; j<(n+m); ++j) {
					B[(((4*(i-offset_r))+j)-n)] = mat[((7*i)+j)];
				}
			}
		}
		__matrix_double_4_3_t C;
		{
			int i;
			int j;
			for (i = n; i<(m+n); ++i) {
				for (j = offset_c; j<(offset_c+n); ++j) {
					C[(((3*(i-n))+j)-offset_c)] = mat[((7*i)+j)];
				}
			}
		}
		__matrix_double_4_4_t D;
		{
			int i;
			int j;
			for (i = n; i<(m+n); ++i) {
				for (j = n; j<(n+m); ++j) {
					D[(((4*i)+j)-(5*n))] = mat[((7*i)+j)];
				}
			}
		}
		__matrix_double_4_4_t S;
		__matrix_double_4_3_t CA_inv;
		{
			int _i;
			int _j;
			int _k;
			for (_i = 0; _i<4; ++_i) {
				for (_j = 0; _j<3; ++_j) {
					CA_inv[((3*_i)+_j)] = 0;
					for (_k = 0; _k<3; ++_k) {
						CA_inv[((3*_i)+_j)] = (CA_inv[((3*_i)+_j)]+(C[((3*_i)+_k)]*A_inv[((3*_k)+_j)]));
					}
				}
			}
		}
		__matrix_double_4_4_t CA_invB;
		{
			int _i;
			int _j;
			int _k;
			for (_i = 0; _i<4; ++_i) {
				for (_j = 0; _j<4; ++_j) {
					CA_invB[((4*_i)+_j)] = 0;
					for (_k = 0; _k<3; ++_k) {
						CA_invB[((4*_i)+_j)] = (CA_invB[((4*_i)+_j)]+(CA_inv[((3*_i)+_k)]*B[((4*_k)+_j)]));
					}
				}
			}
		}
		{
			int _i;
			int _j;
			for (_i = 0; _i<4; ++_i) {
				for (_j = 0; _j<4; ++_j) {
					S[((4*_i)+_j)] = (D[((4*_i)+_j)]-CA_invB[((4*_i)+_j)]);
				}
			}
		}
		__matrix_double_4_4_t S_inv;
		{
			int P[5];
			int i;
			int j;
			int k;
			double tmp_row[4];
			int imax;
			double maxA;
			double absA;
			for (i = 0; i<5; ++i) {
				P[i] = i;
			}
			for (i = 0; i<4; ++i) {
				maxA = 0.0;
				imax = i;
				for (k = i; k<4; ++k) {
					absA = fabs(S[((4*(k+offset_r))+i+offset_c)]);
					if ((absA>maxA)) {
						maxA = absA;
						imax = k;
					}
				}
				if ((maxA<1.0E-4)) {
					printf("[WARNING] Inverting potentially singular matrix\n");
					return -1;
				}
				if ((imax!=i)) {
					j = P[i];
					P[i] = P[imax];
					P[imax] = j;
					std::memcpy((&(tmp_row[0])), (&(S[((4*(i+offset_r))+offset_c)])), (4*sizeof(double)));
					std::memcpy((&(S[((4*(i+offset_r))+offset_c)])), (&(S[((4*(imax+offset_r))+offset_c)])), (4*sizeof(double)));
					std::memcpy((&(S[((4*(imax+offset_r))+offset_c)])), (&(tmp_row[0])), (4*sizeof(double)));
					(P[5]++);
				}
				for (j = (i+1); j<4; ++j) {
					S[((4*(j+offset_r))+i+offset_c)] = (S[((4*(j+offset_r))+i+offset_c)]/S[((5*i)+(4*offset_r)+offset_c)]);
					for (k = (i+1); k<4; ++k) {
						S[((4*(j+offset_r))+k+offset_c)] = (S[((4*(j+offset_r))+k+offset_c)]-(S[((4*(j+offset_r))+i+offset_c)]*S[((4*(i+offset_r))+k+offset_c)]));
					}
				}
			}
			for (j = 0; j<4; ++j) {
				for (i = 0; i<4; ++i) {
					if ((P[i]==j)) {
						S_inv[((4*(i+offset_r))+j+offset_c)] = 1.0;
					} else {
						S_inv[((4*(i+offset_r))+j+offset_c)] = 0.0;
					}
					for (k = 0; k<i; ++k) {
						S_inv[((4*(i+offset_r))+j+offset_c)] = (S_inv[((4*(i+offset_r))+j+offset_c)]-(S[((4*(i+offset_r))+k+offset_c)]*S_inv[((4*(k+offset_r))+j+offset_c)]));
					}
				}
				for (i = 3; i>=0; i--) {
					for (k = (i+1); k<4; ++k) {
						S_inv[((4*(i+offset_r))+j+offset_c)] = (S_inv[((4*(i+offset_r))+j+offset_c)]-(S[((4*(i+offset_r))+k+offset_c)]*S_inv[((4*(k+offset_r))+j+offset_c)]));
					}
					S_inv[((4*(i+offset_r))+j+offset_c)] = (S_inv[((4*(i+offset_r))+j+offset_c)]/S[((5*i)+(4*offset_r)+offset_c)]);
				}
			}
		}
		__matrix_double_3_4_t A_invB;
		{
			int _i;
			int _j;
			int _k;
			for (_i = 0; _i<3; ++_i) {
				for (_j = 0; _j<4; ++_j) {
					A_invB[((4*_i)+_j)] = 0;
					for (_k = 0; _k<3; ++_k) {
						A_invB[((4*_i)+_j)] = (A_invB[((4*_i)+_j)]+(A_inv[((3*_i)+_k)]*B[((4*_k)+_j)]));
					}
				}
			}
		}
		__matrix_double_3_4_t A_invBS_inv;
		{
			int _i;
			int _j;
			int _k;
			for (_i = 0; _i<3; ++_i) {
				for (_j = 0; _j<4; ++_j) {
					A_invBS_inv[((4*_i)+_j)] = 0;
					for (_k = 0; _k<4; ++_k) {
						A_invBS_inv[((4*_i)+_j)] = (A_invBS_inv[((4*_i)+_j)]+(A_invB[((4*_i)+_k)]*S_inv[((4*_k)+_j)]));
					}
				}
			}
		}
		{
			int _i;
			int _j;
			for (_i = 0; _i<3; ++_i) {
				for (_j = 0; _j<4; ++_j) {
					mat_inverse[((7*_i)+_j+3)] = (-A_invBS_inv[((4*_i)+_j)]);
				}
			}
		}
		{
			int i;
			int j;
			for (i = n; i<(n+4); ++i) {
				for (j = n; j<(n+4); ++j) {
					mat_inverse[((7*i)+j)] = S_inv[(((4*i)+j)-(5*n))];
				}
			}
		}
		__matrix_double_4_3_t S_invCA_inv;
		{
			int _i;
			int _j;
			int _k;
			for (_i = 0; _i<4; ++_i) {
				for (_j = 0; _j<3; ++_j) {
					S_invCA_inv[((3*_i)+_j)] = 0;
					for (_k = 0; _k<4; ++_k) {
						S_invCA_inv[((3*_i)+_j)] = (S_invCA_inv[((3*_i)+_j)]+(S_inv[((4*_i)+_k)]*CA_inv[((3*_k)+_j)]));
					}
				}
			}
		}
		{
			int _i;
			int _j;
			for (_i = 0; _i<4; ++_i) {
				for (_j = 0; _j<3; ++_j) {
					mat_inverse[((7*_i)+_j+21)] = (-S_invCA_inv[((3*_i)+_j)]);
				}
			}
		}
		__matrix_double_3_3_t A_invBS_invCA_inv;
		{
			int _i;
			int _j;
			int _k;
			for (_i = 0; _i<3; ++_i) {
				for (_j = 0; _j<3; ++_j) {
					A_invBS_invCA_inv[((3*_i)+_j)] = 0;
					for (_k = 0; _k<4; ++_k) {
						A_invBS_invCA_inv[((3*_i)+_j)] = (A_invBS_invCA_inv[((3*_i)+_j)]+(A_invB[((4*_i)+_k)]*S_invCA_inv[((3*_k)+_j)]));
					}
				}
			}
		}
		{
			int _i;
			int _j;
			for (_i = 0; _i<3; ++_i) {
				for (_j = 0; _j<3; ++_j) {
					mat_inverse[((7*_i)+_j)] = (A_inv[((3*_i)+_j)]+A_invBS_invCA_inv[((3*_i)+_j)]);
				}
			}
		}
	}
	return 0;
}
