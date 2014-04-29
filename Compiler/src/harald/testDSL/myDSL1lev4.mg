def cpu ismootherred_0 ( ) :  Unit 
{ 
loop innerpoints order sred 
u[0] = (((I - ((0.8 * inverse(diag(Laplacian[0]))) * Laplacian[0])) * u[0]) + ((0.8 * inverse(diag(Laplacian[0]))) * f[0]))
next 
}  
def cpu ismootherred_1 ( ) :  Unit 
{ 
loop innerpoints order sred 
u[1] = (((I - ((0.8 * inverse(diag(Laplacian[1]))) * Laplacian[1])) * u[1]) + ((0.8 * inverse(diag(Laplacian[1]))) * f[1]))
next 
}  

def cpu ismootherblack_0 ( ) :  Unit 
{ 
loop innerpoints order sblack 
u[0] = (((I - ((0.8 * inverse(diag(Laplacian[0]))) * Laplacian[0])) * u[0]) + ((0.8 * inverse(diag(Laplacian[0]))) * f[0]))
next 
}  
def cpu ismootherblack_1 ( ) :  Unit 
{ 
loop innerpoints order sblack 
u[1] = (((I - ((0.8 * inverse(diag(Laplacian[1]))) * Laplacian[1])) * u[1]) + ((0.8 * inverse(diag(Laplacian[1]))) * f[1]))
next 
}  

def cpu Jacobi_0 ( ) :  Unit 
{ 
ismootherred_0() 
 ismootherblack_0()
}  
def cpu Jacobi_1 ( ) :  Unit 
{ 
ismootherred_1() 
 ismootherblack_1()
}  

def cpu iprolong_0 ( ) :  Unit 
{ 
loop innerpoints order s 
u[0] = (u[0] + (transpose(R) * u[1]))
next 
}  
def cpu iprolong_1 ( ) :  Unit 
{ 
loop innerpoints order s 
u[1] = (u[1] + (transpose(R) * u[2]))
next 
}  

def cpu iresidual_0 ( ) :  Unit 
{ 
loop innerpoints order s 
res[0] = (f[0] - (Laplacian[0] * u[0]))
next 
}  
def cpu iresidual_1 ( ) :  Unit 
{ 
loop innerpoints order s 
res[1] = (f[1] - (Laplacian[1] * u[1]))
next 
}  

def cpu irestrict_0 ( ) :  Unit 
{ 
loop innerpoints order s 
f[1] = (R * res[0])
next 
}  
def cpu irestrict_1 ( ) :  Unit 
{ 
loop innerpoints order s 
f[2] = (R * res[1])
next 
}  

def cpu icycle_0 ( ) :  Unit 
{ 
repeat up 1 
 (Jacobi_0()) 
 iresidual_0() 
 irestrict_0() 
 repeat up 1 
 (icycle_1()) 
 iprolong_0() 
 repeat up 1 
 (Jacobi_0())}  
def cpu icycle_1 ( ) :  Unit 
{ 
repeat up 8 
 (Jacobi_1())}  

