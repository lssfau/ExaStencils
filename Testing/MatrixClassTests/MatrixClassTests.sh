
#!/bin/bash

configList=""

configList+="./invert/CompileTime/BlockDiagonal/ "
configList+="./invert/CompileTime/Cofactors/ "
configList+="./invert/CompileTime/Diagonal/ "
configList+="./invert/CompileTime/GaussJordan/ "
configList+="./invert/CompileTime/Schur/ "

configList+="./invert/RunTime/BlockDiagonal/ "
configList+="./invert/RunTime/LU/ "
#configList+="./invert/RunTime/Diagonal/ "
configList+="./invert/RunTime/Schur/ "
configList+="./resolvingMatrixFunctions/dot/ "
configList+="./resolvingMatrixFunctions/slicing/ "
configList+="./resolvingMatrixFunctions/transpose/ "