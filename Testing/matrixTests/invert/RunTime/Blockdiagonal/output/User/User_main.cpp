#include "User/User.h"

int main (int argc, char** argv) {
	__matrix_double_4_4_t mat {1.0, 2.0, 0.0, 0.0, 5.0, 6.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 0.0, 0.0, 5.0, 6.0};
	__matrix_double_4_4_t mat_inverse2;
	{
		printf("%f ", mat[0]);
		printf("%f ", mat[1]);
		printf("%f ", mat[2]);
		printf("%f ", mat[3]);
		printf("\n");
		printf("%f ", mat[4]);
		printf("%f ", mat[5]);
		printf("%f ", mat[6]);
		printf("%f ", mat[7]);
		printf("\n");
		printf("%f ", mat[8]);
		printf("%f ", mat[9]);
		printf("%f ", mat[10]);
		printf("%f ", mat[11]);
		printf("\n");
		printf("%f ", mat[12]);
		printf("%f ", mat[13]);
		printf("%f ", mat[14]);
		printf("%f ", mat[15]);
		printf("\n");
		int block;
		for (block = 0; block<4; block = (block+2)) {
			int P[3];
			int i;
			int j;
			int k;
			double tmp_row[2];
			int imax;
			double maxA;
			double absA;
			for (i = 0; i<3; ++i) {
				P[i] = i;
			}
			for (i = 0; i<2; ++i) {
				maxA = 0.0;
				imax = i;
				for (k = i; k<2; ++k) {
					absA = fabs(mat[((5*block)+(4*k)+i)]);
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
					std::memcpy((&(tmp_row[0])), (&(mat[((5*block)+(4*i))])), (2*sizeof(double)));
					std::memcpy((&(mat[((5*block)+(4*i))])), (&(mat[((5*block)+(4*imax))])), (2*sizeof(double)));
					std::memcpy((&(mat[((5*block)+(4*imax))])), (&(tmp_row[0])), (2*sizeof(double)));
					(P[3]++);
				}
				for (j = (i+1); j<2; ++j) {
					mat[((5*block)+(4*j)+i)] = (mat[((5*block)+(4*j)+i)]/mat[(5*(block+i))]);
					for (k = (i+1); k<2; ++k) {
						mat[((5*block)+(4*j)+k)] = (mat[((5*block)+(4*j)+k)]-(mat[((5*block)+(4*j)+i)]*mat[((5*block)+(4*i)+k)]));
					}
				}
			}
			for (j = 0; j<2; ++j) {
				for (i = 0; i<2; ++i) {
					if ((P[i]==j)) {
						mat_inverse2[((5*block)+(4*i)+j)] = 1.0;
					} else {
						mat_inverse2[((5*block)+(4*i)+j)] = 0.0;
					}
					for (k = 0; k<i; ++k) {
						mat_inverse2[((5*block)+(4*i)+j)] = (mat_inverse2[((5*block)+(4*i)+j)]-(mat[((5*block)+(4*i)+k)]*mat_inverse2[((5*block)+(4*k)+j)]));
					}
				}
				for (i = 1; i>=0; i--) {
					for (k = (i+1); k<2; ++k) {
						mat_inverse2[((5*block)+(4*i)+j)] = (mat_inverse2[((5*block)+(4*i)+j)]-(mat[((5*block)+(4*i)+k)]*mat_inverse2[((5*block)+(4*k)+j)]));
					}
					mat_inverse2[((5*block)+(4*i)+j)] = (mat_inverse2[((5*block)+(4*i)+j)]/mat[(5*(block+i))]);
				}
			}
		}
		printf("%f ", mat_inverse2[0]);
		printf("%f ", mat_inverse2[1]);
		printf("%f ", mat_inverse2[2]);
		printf("%f ", mat_inverse2[3]);
		printf("\n");
		printf("%f ", mat_inverse2[4]);
		printf("%f ", mat_inverse2[5]);
		printf("%f ", mat_inverse2[6]);
		printf("%f ", mat_inverse2[7]);
		printf("\n");
		printf("%f ", mat_inverse2[8]);
		printf("%f ", mat_inverse2[9]);
		printf("%f ", mat_inverse2[10]);
		printf("%f ", mat_inverse2[11]);
		printf("\n");
		printf("%f ", mat_inverse2[12]);
		printf("%f ", mat_inverse2[13]);
		printf("%f ", mat_inverse2[14]);
		printf("%f ", mat_inverse2[15]);
		printf("\n");
	}
	return 0;
}
