#include "User/User.h"

int main (int argc, char** argv) {
	MPI_Init(&argc, &argv);
	__matrix_double_2_2_t m1 {1.0, 2.0, 3.0, 4.0};
	__matrix_double_2_2_t m2 {1.0, 2.0, 3.0, 4.0};
	__matrix_double_2_2_t res = ((m1[0]*m2[0])+(m1[1]*m2[1])+(m1[2]*m2[2])+(m1[3]*m2[3]));
	return;
	MPI_Finalize();
	return 0;
}
