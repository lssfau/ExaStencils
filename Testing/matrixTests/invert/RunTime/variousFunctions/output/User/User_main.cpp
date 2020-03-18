#include "User/User.h"

int main (int argc, char** argv) {
	__matrix_double_4_4_t mat {1.0, 2.0, 0.0, 0.0, 5.0, 6.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 0.0, 0.0, 5.0, 6.0};
	__matrix_double_4_4_t mat_inverse2;
	{
		__matrix_int_4_4_t left;
		__matrix_int_4_4_t right;
		int i;
		int j;
		__matrix_int_4_4_t out;
		for (i = 0; i<4; ++i) {
			for (j = 0; j<4; ++j) {
				left[((4*i)+j)] = ((4*i)+j);
				right[((4*i)+j)] = ((4*i)+j);
			}
		}
		printf("[Test] Input matrix 'left'\n");
		printf("%d ", left[0]);
		printf("%d ", left[1]);
		printf("%d ", left[2]);
		printf("%d ", left[3]);
		printf("\n");
		printf("%d ", left[4]);
		printf("%d ", left[5]);
		printf("%d ", left[6]);
		printf("%d ", left[7]);
		printf("\n");
		printf("%d ", left[8]);
		printf("%d ", left[9]);
		printf("%d ", left[10]);
		printf("%d ", left[11]);
		printf("\n");
		printf("%d ", left[12]);
		printf("%d ", left[13]);
		printf("%d ", left[14]);
		printf("%d ", left[15]);
		printf("\n");
		printf("[Test] Input matrix 'right'\n");
		printf("%d ", right[0]);
		printf("%d ", right[1]);
		printf("%d ", right[2]);
		printf("%d ", right[3]);
		printf("\n");
		printf("%d ", right[4]);
		printf("%d ", right[5]);
		printf("%d ", right[6]);
		printf("%d ", right[7]);
		printf("\n");
		printf("%d ", right[8]);
		printf("%d ", right[9]);
		printf("%d ", right[10]);
		printf("%d ", right[11]);
		printf("\n");
		printf("%d ", right[12]);
		printf("%d ", right[13]);
		printf("%d ", right[14]);
		printf("%d ", right[15]);
		printf("\n");
		printf("[Test] calculate product:\n");
		{
			int _i;
			int _j;
			int _k;
			for (_i = 0; _i<4; ++_i) {
				for (_j = 0; _j<4; ++_j) {
					out[((4*_i)+_j)] = 0;
					for (_k = 0; _k<4; ++_k) {
						out[((4*_i)+_j)] = (out[((4*_i)+_j)]+(left[((4*_i)+_k)]*right[((4*_k)+_j)]));
					}
				}
			}
		}
		printf("%d ", out[0]);
		printf("%d ", out[1]);
		printf("%d ", out[2]);
		printf("%d ", out[3]);
		printf("\n");
		printf("%d ", out[4]);
		printf("%d ", out[5]);
		printf("%d ", out[6]);
		printf("%d ", out[7]);
		printf("\n");
		printf("%d ", out[8]);
		printf("%d ", out[9]);
		printf("%d ", out[10]);
		printf("%d ", out[11]);
		printf("\n");
		printf("%d ", out[12]);
		printf("%d ", out[13]);
		printf("%d ", out[14]);
		printf("%d ", out[15]);
		printf("\n");
		printf("[Test] calculate sum(add):\n");
		{
			int _i;
			int _j;
			for (_i = 0; _i<4; ++_i) {
				for (_j = 0; _j<4; ++_j) {
					out[((4*_i)+_j)] = (left[((4*_i)+_j)]+right[((4*_i)+_j)]);
				}
			}
		}
		printf("%d ", out[0]);
		printf("%d ", out[1]);
		printf("%d ", out[2]);
		printf("%d ", out[3]);
		printf("\n");
		printf("%d ", out[4]);
		printf("%d ", out[5]);
		printf("%d ", out[6]);
		printf("%d ", out[7]);
		printf("\n");
		printf("%d ", out[8]);
		printf("%d ", out[9]);
		printf("%d ", out[10]);
		printf("%d ", out[11]);
		printf("\n");
		printf("%d ", out[12]);
		printf("%d ", out[13]);
		printf("%d ", out[14]);
		printf("%d ", out[15]);
		printf("\n");
		printf("[Test] calculate sum(sub):\n");
		{
			int _i;
			int _j;
			for (_i = 0; _i<4; ++_i) {
				for (_j = 0; _j<4; ++_j) {
					out[((4*_i)+_j)] = (left[((4*_i)+_j)]-right[((4*_i)+_j)]);
				}
			}
		}
		printf("%d ", out[0]);
		printf("%d ", out[1]);
		printf("%d ", out[2]);
		printf("%d ", out[3]);
		printf("\n");
		printf("%d ", out[4]);
		printf("%d ", out[5]);
		printf("%d ", out[6]);
		printf("%d ", out[7]);
		printf("\n");
		printf("%d ", out[8]);
		printf("%d ", out[9]);
		printf("%d ", out[10]);
		printf("%d ", out[11]);
		printf("\n");
		printf("%d ", out[12]);
		printf("%d ", out[13]);
		printf("%d ", out[14]);
		printf("%d ", out[15]);
		printf("\n");
		printf("[Test] calculate negative:\n");
		{
			int _i;
			int _j;
			for (_i = 0; _i<4; ++_i) {
				for (_j = 0; _j<4; ++_j) {
					out[((4*_i)+_j)] = (-left[((4*_i)+_j)]);
				}
			}
		}
		printf("%d ", out[0]);
		printf("%d ", out[1]);
		printf("%d ", out[2]);
		printf("%d ", out[3]);
		printf("\n");
		printf("%d ", out[4]);
		printf("%d ", out[5]);
		printf("%d ", out[6]);
		printf("%d ", out[7]);
		printf("\n");
		printf("%d ", out[8]);
		printf("%d ", out[9]);
		printf("%d ", out[10]);
		printf("%d ", out[11]);
		printf("\n");
		printf("%d ", out[12]);
		printf("%d ", out[13]);
		printf("%d ", out[14]);
		printf("%d ", out[15]);
		printf("\n");
		printf("[Test] copy submatrix from 'left' at offset_rows = 1, offset_cols = 2, n_rows = 1, n_cols = 2:\n");
		int offset_r;
		int offset_c;
		int n_rows;
		int n_cols;
		offset_r = 1;
		offset_c = 2;
		n_rows = 1;
		n_cols = 2;
		__matrix_int_1_2_t submatrix;
		{
			int i;
			int j;
			for (i = offset_r; i<(n_rows+offset_r); ++i) {
				for (j = offset_c; j<(offset_c+n_cols); ++j) {
					submatrix[(((2*(i-offset_r))+j)-offset_c)] = left[((4*i)+j)];
				}
			}
		}
		printf("%d ", submatrix[0]);
		printf("%d ", submatrix[1]);
		printf("\n");
		printf("[Test] copy submatrix from 'left' at offset_rows = 1, offset_cols = 1, n_rows = 3, n_cols = 2:\n");
		offset_r = 1;
		offset_c = 1;
		n_rows = 3;
		n_cols = 2;
		__matrix_int_3_2_t submatrix2;
		{
			int i;
			int j;
			for (i = offset_r; i<(n_rows+offset_r); ++i) {
				for (j = offset_c; j<(offset_c+n_cols); ++j) {
					submatrix2[(((2*(i-offset_r))+j)-offset_c)] = left[((4*i)+j)];
				}
			}
		}
		printf("%d ", submatrix2[0]);
		printf("%d ", submatrix2[1]);
		printf("\n");
		printf("%d ", submatrix2[2]);
		printf("%d ", submatrix2[3]);
		printf("\n");
		printf("%d ", submatrix2[4]);
		printf("%d ", submatrix2[5]);
		printf("\n");
		printf("[Test] write matrix at offset_rows = 1, offset_cols = 2, n_rows = 3, n_cols = 1 to right:\n");
		__matrix_int_3_1_t mat;
		printf("%d ", mat[0]);
		printf("\n");
		printf("%d ", mat[1]);
		printf("\n");
		printf("%d ", mat[2]);
		printf("\n");
		offset_c = 2;
		{
			int i;
			int j;
			for (i = offset_r; i<(offset_r+3); ++i) {
				for (j = offset_c; j<(offset_c+1); ++j) {
					right[((4*i)+j)] = mat[((i+j)-(offset_c+offset_r))];
				}
			}
		}
		printf("%d ", right[0]);
		printf("%d ", right[1]);
		printf("%d ", right[2]);
		printf("%d ", right[3]);
		printf("\n");
		printf("%d ", right[4]);
		printf("%d ", right[5]);
		printf("%d ", right[6]);
		printf("%d ", right[7]);
		printf("\n");
		printf("%d ", right[8]);
		printf("%d ", right[9]);
		printf("%d ", right[10]);
		printf("%d ", right[11]);
		printf("\n");
		printf("%d ", right[12]);
		printf("%d ", right[13]);
		printf("%d ", right[14]);
		printf("%d ", right[15]);
		printf("\n");
		printf("[Test] write matrix at offset_rows = 1, offset_cols = 1, n_rows = 3, n_cols = 3 to right:\n");
		__matrix_int_3_3_t mat2;
		printf("%d ", mat2[0]);
		printf("%d ", mat2[1]);
		printf("%d ", mat2[2]);
		printf("\n");
		printf("%d ", mat2[3]);
		printf("%d ", mat2[4]);
		printf("%d ", mat2[5]);
		printf("\n");
		printf("%d ", mat2[6]);
		printf("%d ", mat2[7]);
		printf("%d ", mat2[8]);
		printf("\n");
		offset_c = 1;
		{
			int i;
			int j;
			for (i = offset_r; i<(offset_r+3); ++i) {
				for (j = offset_c; j<(offset_c+3); ++j) {
					right[((4*i)+j)] = mat2[(((3*(i-offset_r))+j)-offset_c)];
				}
			}
		}
		printf("%d ", right[0]);
		printf("%d ", right[1]);
		printf("%d ", right[2]);
		printf("%d ", right[3]);
		printf("\n");
		printf("%d ", right[4]);
		printf("%d ", right[5]);
		printf("%d ", right[6]);
		printf("%d ", right[7]);
		printf("\n");
		printf("%d ", right[8]);
		printf("%d ", right[9]);
		printf("%d ", right[10]);
		printf("%d ", right[11]);
		printf("\n");
		printf("%d ", right[12]);
		printf("%d ", right[13]);
		printf("%d ", right[14]);
		printf("%d ", right[15]);
		printf("\n");
	}
	return 0;
}
