
#!/bin/bash

configList=""

#compiletime inverse
configList+="./invert/CompileTime/BlockDiagonal/ "
configList+="./invert/CompileTime/Cofactors/ "
configList+="./invert/CompileTime/Diagonal/ "
#configList+="./invert/CompileTime/GaussJordan/ "
configList+="./invert/CompileTime/Schur/ "

#runtime inverse
configList+="./invert/RunTime/BlockDiagonal/ "
configList+="./invert/RunTime/LU/ "
configList+="./invert/RunTime/Diagonal/ "
configList+="./invert/RunTime/Schur/ "
configList+="./invert/RunTime/SmallMatrix/ "
configList+="./invert/RunTime/SchurLargeMatrix/ "

# built-in functions
configList+="./resolvingMatrixFunctions/dot/ "
configList+="./resolvingMatrixFunctions/slicing/ "
configList+="./resolvingMatrixFunctions/transpose/ "
configList+="./resolvingMatrixFunctions/cross/ "
configList+="./resolvingMatrixFunctions/determinant/ "
configList+="./resolvingMatrixFunctions/trace/ "
configList+="./resolvingMatrixFunctions/chaines/ "

#operators
configList+="./resolvingMatrixOperators/ "

#others
configList+="./resolvingMatrixAssignments/ "
configList+="./determineMatrixStructures/ "