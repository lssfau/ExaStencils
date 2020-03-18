#include "User/User.h"
void print(const int n, __matrix_double_3_3_t mat)
{
	for (int i = 0; i < n; ++i)
	{
		for (int j = 0; j < n; ++j)
		{
			std::cout << mat[i * n + j] << ' ';
		}
		std::cout << '\n';
	}

}
int main (int argc, char** argv) {
	__matrix_double_7_7_t mat {1.0, 2.0, 1.0, 0.0, 0.0, 0.0, 3.0, 4.0, 2.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 5.0, 1.0, 0.0, 0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 4.0, 1.0, 3.0, 2.0, 0.0, 0.0, 0.0, 1.0, 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 4.0, 6.0, 7.0, 3.0, 3.0, 5.0, 7.0, 4.0, 4.0, 5.0, 1.0};
	__matrix_double_3_3_t mat_inverse {((mat[1]*mat[31])+(mat[0]*mat[24])+(mat[2]*mat[38])), ((mat[1]*mat[32])+(mat[0]*mat[25])+(mat[2]*mat[39])), ((mat[1]*mat[33])+(mat[0]*mat[26])+(mat[2]*mat[40])), ((mat[8]*mat[31])+(mat[7]*mat[24])+(mat[9]*mat[38])), ((mat[8]*mat[32])+(mat[7]*mat[25])+(mat[9]*mat[39])), ((mat[8]*mat[33])+(mat[7]*mat[26])+(mat[9]*mat[40])), ((mat[15]*mat[31])+(mat[14]*mat[24])+(mat[16]*mat[38])), ((mat[15]*mat[32])+(mat[14]*mat[25])+(mat[16]*mat[39])), ((mat[15]*mat[33])+(mat[14]*mat[26])+(mat[16]*mat[40]))};
	print(3, mat_inverse);
	return 0;
}
