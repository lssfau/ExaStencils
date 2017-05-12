set configurations=MPI_4_OMP_2 MPI_8 OMP_8 CUDA
:: MPI_4_OMP_2 MPI_8 OMP_8 CUDA

for %%c in (%configurations%) do (
	java.exe -Xmx2G -Xms2G -cp "C:\Eclipse\plugins\*;.\Compiler\bin;.\CompilerMacros\bin;.\Compiler\lib\*" Main .\Configs\NNF2\i10staff40\%%c.settings .\Configs\NNF2\%%c.knowledge .\Configs\Sebastian\i10staff40.platform
)
