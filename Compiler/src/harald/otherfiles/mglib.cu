#include <cuda.h>
#include <cuda_runtime.h>

unsigned int nextPow2(unsigned int x)
{
    --x;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    return ++x;
}

void getNumBlocksAndThreads(int whichKernel, int n, int maxBlocks, int maxThreads, int &blocks, int &threads)
{
    //get device capability, to avoid block/grid size excceed the upbound
    static bool firstRun = 1;
	static cudaDeviceProp prop;
	if (firstRun)
	{
		int device;
		(cudaGetDevice(&device));
		(cudaGetDeviceProperties(&prop, device));
	}

    if (whichKernel < 3)
    {
        threads = (n < maxThreads) ? nextPow2(n) : maxThreads;
        blocks = (n + threads - 1) / threads;
    }
    else
    {
        threads = (n < maxThreads*2) ? nextPow2((n + 1)/ 2) : maxThreads;
        blocks = (n + (threads * 2 - 1)) / (threads * 2);
    }

    if (blocks > prop.maxGridSize[0])
    {
        blocks /= 2;
        threads *= 2;
    }

    if (whichKernel == 6)
    {
        blocks = min(maxBlocks, blocks);
    }
}


template<class T>
struct SharedMemory
{
    __device__ inline operator       T *()
    {
        extern __shared__ int __smem[];
        return (T *)__smem;
    }

    __device__ inline operator const T *() const
    {
        extern __shared__ int __smem[];
        return (T *)__smem;
    }
};


// A.1, usual reduction kernel

/*
    This version adds multiple elements per thread sequentially.  This reduces the overall
    cost of the algorithm while keeping the work complexity O(n) and the step complexity O(log n).
    (Brent's Theorem optimization)

    Note, this kernel needs a minimum of 64*sizeof(T) bytes of shared memory.
    In other words if blockSize <= 32, allocate 64*sizeof(T) bytes.
    If blockSize > 32, allocate blockSize*sizeof(T) bytes.
*/
template <class T, unsigned int blockSize, bool nIsPow2>
__global__ void
reduce6(T *g_idata, T *g_odata, unsigned int n)
{
    T *sdata = SharedMemory<T>();

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = threadIdx.x;
    unsigned int i = blockIdx.x*blockSize*2 + threadIdx.x;
    unsigned int gridSize = blockSize*2*gridDim.x;

    T mySum = 0;

    // we reduce multiple elements per thread.  The number is determined by the
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < n)
    {
        mySum += g_idata[i];

        // ensure we don't read out of bounds -- this is optimized away for powerOf2 sized arrays
        if (nIsPow2 || i + blockSize < n)
            mySum += g_idata[i+blockSize];

        i += gridSize;
    }

    // each thread puts its local sum into shared memory
    sdata[tid] = mySum;
    __syncthreads();


    // do reduction in shared mem
    if (blockSize >= 512)
    {
        if (tid < 256)
        {
            sdata[tid] = mySum = mySum + sdata[tid + 256];
        }

        __syncthreads();
    }

    if (blockSize >= 256)
    {
        if (tid < 128)
        {
            sdata[tid] = mySum = mySum + sdata[tid + 128];
        }

        __syncthreads();
    }

    if (blockSize >= 128)
    {
        if (tid <  64)
        {
            sdata[tid] = mySum = mySum + sdata[tid +  64];
        }

        __syncthreads();
    }

    if (tid < 32)
    {
        // now that we are using warp-synchronous programming (below)
        // we need to declare our shared memory volatile so that the compiler
        // doesn't reorder stores to it and induce incorrect behavior.
        volatile T *smem = sdata;

        if (blockSize >=  64)
        {
            smem[tid] = mySum = mySum + smem[tid + 32];
        }

        if (blockSize >=  32)
        {
            smem[tid] = mySum = mySum + smem[tid + 16];
        }

        if (blockSize >=  16)
        {
            smem[tid] = mySum = mySum + smem[tid +  8];
        }

        if (blockSize >=   8)
        {
            smem[tid] = mySum = mySum + smem[tid +  4];
        }

        if (blockSize >=   4)
        {
            smem[tid] = mySum = mySum + smem[tid +  2];
        }

        if (blockSize >=   2)
        {
            smem[tid] = mySum = mySum + smem[tid +  1];
        }
    }

    // write result for this block to global mem
    if (tid == 0)
        g_odata[blockIdx.x] = sdata[0];
}

template<class T> T fasterReduce (T* data, unsigned int numElements, T* scratch)
{
	int size = numElements;
	int maxThreads = 512;
	int whichKernel = 6;
	int maxBlocks = 1; // 64

    int numBlocks = 0;
    int numThreads = 0;
    getNumBlocksAndThreads(whichKernel, size, maxBlocks, maxThreads, numBlocks, numThreads);

    dim3 dimBlock(numThreads, 1, 1);
    dim3 dimGrid(numBlocks, 1, 1);
	int smemSize = (numThreads <= 32) ? 2 * numThreads * sizeof(T) : numThreads * sizeof(T);


	           switch (numThreads)
                {
                    case 512:
                        reduce6<T, 512, false ><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                    case 256:
                        reduce6<T, 256, false><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                    case 128:
                        reduce6<T, 128, false><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                    case 64:
                        reduce6<T,  64, false><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                    case 32:
                        reduce6<T,  32, false><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                    case 16:
                        reduce6<T,  16, false><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                    case  8:
                        reduce6<T,   8, false><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                    case  4:
                        reduce6<T,   4, false><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                    case  2:
                        reduce6<T,   2, false><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                    case  1:
                        reduce6<T,   1, false><<< dimGrid, dimBlock, smemSize >>>(data, scratch, size); break;
                }

	T result;

	if (numBlocks > 1) {
	//std::cout << numBlocks << std::endl;
	
		size = numBlocks;
		maxBlocks = 1;
        numBlocks = 0;
        numThreads = 0;
        getNumBlocksAndThreads(whichKernel, size, maxBlocks, maxThreads, numBlocks, numThreads);

        dim3 dimBlock(numThreads, 1, 1);
        dim3 dimGrid(numBlocks, 1, 1);
	    int smemSize = (numThreads <= 32) ? 2 * numThreads * sizeof(T) : numThreads * sizeof(T);

	           switch (numThreads)
                {
                    case 512:
                        reduce6<T, 512, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                    case 256:
                        reduce6<T, 256, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                    case 128:
                        reduce6<T, 128, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                    case 64:
                        reduce6<T,  64, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                    case 32:
                        reduce6<T,  32, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                    case 16:
                        reduce6<T,  16, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                    case  8:
                        reduce6<T,   8, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                    case  4:
                        reduce6<T,   4, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                    case  2:
                        reduce6<T,   2, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                    case  1:
                        reduce6<T,   1, false><<< dimGrid, dimBlock, smemSize >>>(scratch, scratch + size, size); break;
                }
				cudaMemcpy(&result, scratch+size, sizeof(T), cudaMemcpyDeviceToHost);
	} else
		cudaMemcpy(&result, scratch, sizeof(T), cudaMemcpyDeviceToHost);

	return result;
}

__device__ __inline__ unsigned int getAbsIdx2(unsigned int x, unsigned int y, unsigned int numX)
{ return x * numX + y; }

template<class T> __global__ void	LU_solve(T* rhs,T* A,T* x, unsigned int n) {  //return x for Ax+b=0 solved with LU-decomp.

 	    //dim3 cellIdx(blockIdx.x * blockDim.x + threadIdx.x, blockIdx.y * blockDim.y + threadIdx.y);

		T b[6];
		int i,j,k=0;

		for (k = 0; k < n; k++)
		b[k] = rhs[k];

		T max = 0.0;
		T s = 0.0;
		T q = 0.0;
		T h = 0.0;

		int p[5]; // ! [n-1];
		for (k = 0; k < n-1; k++){
			max = 0;
			for (i = k; i < n; i++){
				s = 0;
				for (j = k; j < n; j++){
					s += fabs(A[getAbsIdx2(i,j,6)]);
				}
				q = fabs(A[getAbsIdx2(i,k,6)])/s;
				if (q>max){
					max=q;
					p[k]=i;
				}
			}
			if (max==0) break;
			if (p[k]!=k){
				for (j = 0; j < n; j++){
					h = A[getAbsIdx2(k,j,6)];
					A[getAbsIdx2(k,j,6)]=A[getAbsIdx2(p[k],j,6)];
					A[getAbsIdx2(p[k],j,6)]=h;
				}
			}
			for (i = k+1; i < n; i++){
				A[getAbsIdx2(i,k,6)] /= A[getAbsIdx2(k,k,6)];
				for (j = k+1; j < n; j++){
					A[getAbsIdx2(i,j,6)]-=A[getAbsIdx2(i,k,6)]*A[getAbsIdx2(k,j,6)];
				}
			}
		}
		// forward substitution
		T c[6];
		for (k= 0; k < n-1; k++){
			if (p[k]!=k){ //revert Permutations!
				h=b[k];
				b[k]=b[ p[k] ];
				b[ p[k] ]=h;
			}
		}
		for (i=0; i<n; i++){
			c[i]=b[i];
			for (j=0; j<i; j++){
				c[i]-=A[getAbsIdx2(i,j,6)]*c[j];
			}
		}
		// backward substitution
		//T x[6];

		for (i=n-1; i>=0; i--){
			s=c[i];
			for (k=i+1; k < n; k++){
				s += A[getAbsIdx2(i,k,6)] * x[k]; 
			}
			x[i] = -s/A[getAbsIdx2(i,i,6)];
		}


		// descent
		
		T descent;
		for (i=0; i<6; i++)
		  descent += x[i]*rhs[i];

		if (descent > 0)
		  for (i=0; i<6; i++)
		    x[i] = -x[i];
			
	};

// C.5 calls kernel to do LU_solve

template<class T> void LUSolveWrapper (dim3 blockSize, dim3 threadSize, T* rhs, T* A, T* x)
{
   if ((threadSize.x > 1) || (threadSize.y > 1) || (threadSize.x*threadSize.y > 1))
      std::cout << " too many threads per block " << std::endl;

    LU_solve<<<blockSize, threadSize>>>(rhs,A,x, 6);
}


template<class T> __device__  __inline__ T frac (T val)
{ return val - floor(val); }

template<class T> __device__ __inline__ T lerp (T a, T b, T t)
{ return a + t * (b - a); }


template<class T> void pullDataFromDevice (T* cpu, T* gpu, int size)
	{ cudaMemcpy(cpu, gpu, size * sizeof(T), cudaMemcpyDeviceToHost); }

template<class T> void pushDataToDevice (T* cpu, T* gpu, int size)
	{ cudaMemcpy(gpu, cpu, size * sizeof(T), cudaMemcpyHostToDevice); }

	/*
template<class T> class MyArrayCuda;
template<class T> class MyArray;

void printcuda ( int lev,MyArrayCuda<double>& arrcuda, MyArray<double>& arr )  { 
std::cout << "print" << std::endl; 
pullDataFromDevice (arr.begin(), arrcuda.begin(), arr.x1_*arr.x2_);
{for ( int i0 = 0; i0 < arr.x1_;++i0) {
for ( int i1 = 0; i1 < arr.x2_;++i1)
 { 
std::cout << arr(i0,i1) << " " ;}
  std::cout << std::endl;
 }} 
 } 
 */

 /*
__device__ float2 compcoeff ( unsigned int global_idx, float* Sol_im)
{

	float sigma = 1.0f, kdiff = 2, sigmakdiffinv = 1.0f/(kdiff*sigma*kdiff*sigma);
  	sigma = (3.14f/180.0f)*sigma;

	float denominv = 1.0f/( 1.0f + Sol_im[global_idx]*Sol_im[global_idx] * sigmakdiffinv);
    

	float2 v;
	v.x = __cosf(sigma)*denominv;
	v.y = __sinf(sigma)*denominv;
	return v;

}

__device__ float2 mult (float2 v1, float2 v2) {
	float2 r;
	r.x = v1.x*v2.x - v1.y*v2.y;
	r.y = v1.x*v2.y + v2.x*v1.y;
	return r;
}

__device__ float2 mult (float2 v1, float v2_1, float v2_2) {
	float2 v2;
	v2.x = v2_1;
	v2.y = v2_2;
	float2 r;
	r.x = v1.x*v2.x - v1.y*v2.y;
	r.y = v1.x*v2.y + v2.x*v1.y;
	return r;
}

__device__ float mult0 (float2 v1, float v2_1, float v2_2) {
	float2 v2;
	v2.x = v2_1;
	v2.y = v2_2;
	return v1.x*v2.x - v1.y*v2.y;
}

__device__ float mult1 (float2 v1, float v2_1, float v2_2) {
	float2 v2;
	v2.x = v2_1;
	v2.y = v2_2;
	return 	v1.x*v2.y + v2.x*v1.y;
}

__device__ float2 div (float2 v1, float2 v2) {
	float2 r;
    float inv = 1.0f/(v2.x*v2.x+v2.y*v2.y); 
	r.x = inv*(v1.x*v2.x + v1.y*v2.y);
	r.y = inv*(v1.y*v2.x - v1.x*v2.y);
	return r;
}

__device__ float inverse0(float2 v2) {
    float inv = 1.0f/(v2.x*v2.x+v2.y*v2.y); 
	return inv*(v2.x);
}

__device__ float inverse1(float2 v2) {
    float inv = 1.0f/(v2.x*v2.x+v2.y*v2.y); 
	return inv*(v2.y);
}

__device__ void compst ( float2* localst, unsigned int global_idx, float* Sol_im, unsigned int ncols, float fac)
{

    const float timef = 0.01; // 1.0 / tau   
	float2 h;

			h = compcoeff(global_idx, Sol_im);

			localst[1] = compcoeff(global_idx-1, Sol_im );
            localst[1].x += h.x;
            localst[1].y += h.y;
			localst[2] = compcoeff(global_idx+1, Sol_im );
            localst[2].x += h.x;
            localst[2].y += h.y;
			localst[3] = compcoeff(global_idx-ncols, Sol_im );
            localst[3].x += h.x;
            localst[3].y += h.y;
			localst[4] = compcoeff(global_idx+ncols, Sol_im );
            localst[4].x += h.x;
            localst[4].y += h.y;

	        localst[0].x = ( localst[1].x + localst[2].x + localst[3].x + localst[4].x ) *fac + timef;
	        localst[0].y = ( localst[1].y + localst[2].y + localst[3].y + localst[4].y ) *fac + timef;
}

__device__ void compst3D ( float2* localst, unsigned int global_idx, float* Sol_im, unsigned int ncols,unsigned int nrows, float fac)
{

    const float timef = 0.01; // 1.0 / tau   
	float2 h;

			h = compcoeff(global_idx, Sol_im);

			localst[1] = compcoeff(global_idx-1, Sol_im );
            localst[1].x += h.x;
            localst[1].y += h.y;
			localst[2] = compcoeff(global_idx+1, Sol_im );
            localst[2].x += h.x;
            localst[2].y += h.y;
			localst[3] = compcoeff(global_idx-ncols, Sol_im );
            localst[3].x += h.x;
            localst[3].y += h.y;
			localst[4] = compcoeff(global_idx+ncols, Sol_im );
            localst[4].x += h.x;
            localst[4].y += h.y;
			localst[5] = compcoeff(global_idx-ncols*nrows, Sol_im );
            localst[5].x += h.x;
            localst[5].y += h.y;
			localst[6] = compcoeff(global_idx+ncols*nrows, Sol_im );
            localst[6].x += h.x;
            localst[6].y += h.y;

	        localst[0].x = ( localst[1].x + localst[2].x + localst[3].x + localst[4].x + localst[5].x + localst[6].x) *fac + timef;
	        localst[0].y = ( localst[1].y + localst[2].y + localst[3].y + localst[4].y + localst[5].x + localst[6].x) *fac + timef;
}

*/