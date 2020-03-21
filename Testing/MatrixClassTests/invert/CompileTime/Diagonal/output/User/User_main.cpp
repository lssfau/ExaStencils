#include "User/User.h"
void print(double* m, int s) {
	for (int i = 0; i < s; ++i) {
		for (int j = 0; j < s; ++j) {
			std::cout << m[i * s + j] << " ";
		}
		std::cout << std::endl;
	}
}
int main (int argc, char** argv) {
	__matrix_double_4_4_t mat {1.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 0.0, 4.0};
	__matrix_double_4_4_t mat_inverse {(1.0/mat[0]), mat[1], mat[2], mat[3], mat[4], (1.0/mat[5]), mat[6], mat[7], mat[8], mat[9], (1.0/mat[10]), mat[11], mat[12], mat[13], mat[14], (1.0/mat[15])};
	__matrix_double_6_6_t mat2 {1.123, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.156, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.135, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.532, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.131, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.213};
	__matrix_double_6_6_t mat_inverse2 {(1.0/mat2[0]), mat2[1], mat2[2], mat2[3], mat2[4], mat2[5], mat2[6], (1.0/mat2[7]), mat2[8], mat2[9], mat2[10], mat2[11], mat2[12], mat2[13], (1.0/mat2[14]), mat2[15], mat2[16], mat2[17], mat2[18], mat2[19], mat2[20], (1.0/mat2[21]), mat2[22], mat2[23], mat2[24], mat2[25], mat2[26], mat2[27], (1.0/mat2[28]), mat2[29], mat2[30], mat2[31], mat2[32], mat2[33], mat2[34], (1.0/mat2[35])};
	print(mat2, 6);
	print(mat_inverse2, 6);

		return 0;
}
