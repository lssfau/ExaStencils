#include "User/User.h"

int main (int argc, char** argv) {
	__matrix_double_4_4_t mat {1.0, 2.0, 3.0, 1.0, 5.0, 6.0, 4.0, 1.0, 9.0, 1.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
	__matrix_double_4_4_t mat_inverse2;
	{
		printf("%e ", mat[0]);
		printf("%e ", mat[1]);
		printf("%e ", mat[2]);
		printf("%e ", mat[3]);
		printf("\n");
		printf("%e ", mat[4]);
		printf("%e ", mat[5]);
		printf("%e ", mat[6]);
		printf("%e ", mat[7]);
		printf("\n");
		printf("%e ", mat[8]);
		printf("%e ", mat[9]);
		printf("%e ", mat[10]);
		printf("%e ", mat[11]);
		printf("\n");
		printf("%e ", mat[12]);
		printf("%e ", mat[13]);
		printf("%e ", mat[14]);
		printf("%e ", mat[15]);
		printf("\n");
		int i;
		int j;
		int k;
		int off_c;
		int off_r;
		off_c = 2;
		off_r = 2;
		int block_s;
		block_s = 2;
		double tmp_row[2];
		int imax;
		double maxA;
		double absA;
		int P[3];
		for (i = 0; i<(block_s+1); ++i) {
			P[i] = i;
		}
		for (i = 0; i<block_s; ++i) {
			maxA = 0.0;
			imax = i;
			for (k = i; k<block_s; ++k) {
				absA = fabs(mat[((4*(k+off_r))+i+off_c)]);
				if ((absA>maxA)) {
					maxA = absA;
					imax = k;
				}
			}
			if ((maxA<1.0E-4)) {
				return -1;
			}
			if ((imax!=i)) {
				j = P[i];
				P[i] = P[imax];
				P[imax] = j;
				std::memcpy((&(tmp_row[0])), (&(mat[((4*(i+off_r))+off_c)])), (block_s*sizeof(double)));
				std::memcpy((&(mat[((4*(i+off_r))+off_c)])), (&(mat[((4*(imax+off_r))+off_c)])), (block_s*sizeof(double)));
				std::memcpy((&(mat[((4*(imax+off_r))+off_c)])), (&(tmp_row[0])), (block_s*sizeof(double)));
				(P[(block_s+1)]++);
			}
			for (j = (i+1); j<block_s; ++j) {
				mat[((4*(j+off_r))+i+off_c)] = (mat[((4*(j+off_r))+i+off_c)]/mat[((5*i)+(4*off_r)+off_c)]);
				for (k = (i+1); k<block_s; ++k) {
					mat[((4*(j+off_r))+k+off_c)] = (mat[((4*(j+off_r))+k+off_c)]-(mat[((4*(j+off_r))+i+off_c)]*mat[((4*(i+off_r))+k+off_c)]));
				}
			}
		}
		printf("%e ", mat[0]);
		printf("%e ", mat[1]);
		printf("%e ", mat[2]);
		printf("%e ", mat[3]);
		printf("\n");
		printf("%e ", mat[4]);
		printf("%e ", mat[5]);
		printf("%e ", mat[6]);
		printf("%e ", mat[7]);
		printf("\n");
		printf("%e ", mat[8]);
		printf("%e ", mat[9]);
		printf("%e ", mat[10]);
		printf("%e ", mat[11]);
		printf("\n");
		printf("%e ", mat[12]);
		printf("%e ", mat[13]);
		printf("%e ", mat[14]);
		printf("%e ", mat[15]);
		printf("\n");
	}
	return 0;
}
