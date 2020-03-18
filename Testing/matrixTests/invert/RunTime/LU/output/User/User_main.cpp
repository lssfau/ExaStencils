#include "User/User.h"

int main (int argc, char** argv) {
	__matrix_double_4_4_t mat {1.0, 2.0, 3.0, 1.0, 5.0, 6.0, 4.0, 1.0, 9.0, 1.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
	__matrix_double_4_4_t mat_inverse2;
	{
		__matrix_double_4_4_t _L;
		__matrix_double_4_4_t _U;
		std::fill(_L, (_L+16), 0.0);
		std::copy(mat, (mat+16), _U);
		int _q[4];
		std::fill(mat_inverse2, (mat_inverse2+16), 0.0);
		_L[0] = 1.0;
		_L[5] = 1.0;
		_L[10] = 1.0;
		_L[15] = 1.0;
		_q[0] = 0;
		_q[1] = 1;
		_q[2] = 2;
		_q[3] = 3;
		for (int _k = 0; _k<3; ++_k) {
			double _colmax = 0.0;
			int _maxCol = _k;
			for (int _j = _k; _j<4; ++_j) {
				if ((std::fabs(_U[((4*_k)+_j)])>_colmax)) {
					_colmax = std::fabs(_U[((4*_k)+_j)]);
					_maxCol = _j;
				}
			}
			
			std::swap(_q[_k], _q[_maxCol]);
			std::swap(_L[_k], _L[_maxCol]);
			std::swap(_L[(_k+4)], _L[(_maxCol+4)]);
			std::swap(_L[(_k+8)], _L[(_maxCol+8)]);
			std::swap(_L[(_k+12)], _L[(_maxCol+12)]);
			std::swap(_U[_k], _U[_maxCol]);
			std::swap(_U[(_k+4)], _U[(_maxCol+4)]);
			std::swap(_U[(_k+8)], _U[(_maxCol+8)]);
			std::swap(_U[(_k+12)], _U[(_maxCol+12)]);
			for (int _i = (_k+1); _i<4; ++_i) {
				_L[((4*_i)+_k)] = (_U[((4*_i)+_k)]/_U[(5*_k)]);
				for (int _j = _k; _j<4; ++_j) {
					_U[((4*_i)+_j)] = (_U[((4*_i)+_j)]-(_L[((4*_i)+_k)]*_U[((4*_k)+_j)]));
				}
			}
		}
		__matrix_double_4_4_t _y;
		for (int _j = 0; _j<4; ++_j) {
			for (int _i = 0; _i<4; ++_i) {
				double _sum = 0.0;
				for (int _k = 0; _k<_i; ++_k) {
					_sum = (_sum+(_L[((4*_i)+_k)]*_y[((4*_k)+_j)]));
				}
				if ((_i==_j)) {
					_y[((4*_i)+_j)] = ((1.0-_sum)/_L[(5*_i)]);
				} else {
					_y[((4*_i)+_j)] = (-(_sum/_L[(5*_i)]));
				}
			}
			for (int _i = 3; _i>=0; --_i) {
				double _sum = 0.0;
				for (int _k = 3; _k>_i; --_k) {
					_sum = (_sum+(_U[((4*_i)+_k)]*mat_inverse2[((4*_k)+_j)]));
				}
				mat_inverse2[((4*_i)+_j)] = ((_y[((4*_i)+_j)]-_sum)/_U[(5*_i)]);
			}
		}
		printf("%e ", mat_inverse2[0]);
		printf("%e ", mat_inverse2[1]);
		printf("%e ", mat_inverse2[2]);
		printf("%e ", mat_inverse2[3]);
		printf("\n");
		printf("%e ", mat_inverse2[4]);
		printf("%e ", mat_inverse2[5]);
		printf("%e ", mat_inverse2[6]);
		printf("%e ", mat_inverse2[7]);
		printf("\n");
		printf("%e ", mat_inverse2[8]);
		printf("%e ", mat_inverse2[9]);
		printf("%e ", mat_inverse2[10]);
		printf("%e ", mat_inverse2[11]);
		printf("\n");
		printf("%e ", mat_inverse2[12]);
		printf("%e ", mat_inverse2[13]);
		printf("%e ", mat_inverse2[14]);
		printf("%e ", mat_inverse2[15]);
		printf("\n");
	}
	return 0;
}
