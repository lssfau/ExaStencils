#include "User/User.h"

int main (int argc, char** argv) {
	__matrix_double_2_2_t m1 {1.0, 2.0, 3.0, 4.0};
	__matrix_double_2_2_t m1_inverse_split;
	{
		int zero = 0;
		{
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
					absA = fabs((m1[(k+zero), (i+zero)]));
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
					std::memcpy((&(tmp_row[0])), (&((m1[(i+zero), zero]))), (2*sizeof(double)));
					std::memcpy((&((m1[(i+zero), zero]))), (&((m1[(imax+zero), zero]))), (2*sizeof(double)));
					std::memcpy((&((m1[(imax+zero), zero]))), (&(tmp_row[0])), (2*sizeof(double)));
					(P[3]++);
				}
				for (j = (i+1); j<2; ++j) {
					(m1[(j+zero), (i+zero)]) = ((m1[(j+zero), (i+zero)])/(m1[(i+zero), (i+zero)]));
					for (k = (i+1); k<2; ++k) {
						(m1[(j+zero), (k+zero)]) = ((m1[(j+zero), (k+zero)])-((m1[(j+zero), (i+zero)])*(m1[(i+zero), (k+zero)])));
					}
				}
			}
			for (j = 0; j<2; ++j) {
				for (i = 0; i<2; ++i) {
					if ((P[i]==j)) {
						(m1_inverse_split[(i+zero), (j+zero)]) = 1.0;
					} else {
						(m1_inverse_split[(i+zero), (j+zero)]) = 0.0;
					}
					for (k = 0; k<i; ++k) {
						(m1_inverse_split[(i+zero), (j+zero)]) = ((m1_inverse_split[(i+zero), (j+zero)])-((m1[(i+zero), (k+zero)])*(m1_inverse_split[(k+zero), (j+zero)])));
					}
				}
				for (i = 1; i>=0; i--) {
					for (k = (i+1); k<2; ++k) {
						(m1_inverse_split[(i+zero), (j+zero)]) = ((m1_inverse_split[(i+zero), (j+zero)])-((m1[(i+zero), (k+zero)])*(m1_inverse_split[(k+zero), (j+zero)])));
					}
					(m1_inverse_split[(i+zero), (j+zero)]) = ((m1_inverse_split[(i+zero), (j+zero)])/(m1[(i+zero), (i+zero)]));
				}
			}
		}
		printf("%f ", (m1_inverse_split[0, 0]));
		printf("%f ", (m1_inverse_split[0, 1]));
		printf("\n");
		printf("%f ", (m1_inverse_split[1, 0]));
		printf("%f ", (m1_inverse_split[1, 1]));
		printf("\n");
	}
	return 0;
}
