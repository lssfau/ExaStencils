set configurations=MPI_8_OMP_8
::MPI_32 OMP_32 MPI_4_OMP_8 MPI_64 MPI_8_OMP_8

for %%c in (%configurations%) do (
	java.exe -Xmx2G -Xms2G -cp "C:\Eclipse\plugins\*;.\Compiler\bin;.\CompilerMacros\bin;.\Compiler\lib\*" Main .\Configs\NNF2\%%c.settings .\Configs\NNF2\%%c.knowledge .\Configs\Sebastian\i10hpc.platform
)

::java.exe -Xmx2G -Xms2G -cp "C:\Eclipse\plugins\*;.\Compiler\bin;.\CompilerMacros\bin;.\Compiler\lib\*" Main .\Configs\NNF2\CUDA.settings .\Configs\NNF2\CUDA.knowledge .\Configs\Sebastian\i10hpc_gcc.platform
