VEC res SIZE u 
MATRIX N SIZE Laplacian 
MATRIX M SIZE Laplacian 
RESTRMATRIX R of u = 2 
SET s = [0,0][1,1] 
SET sred = [0,0][2,2] [1,1][2,2] 
SET sblack = [0,1][2,2] [1,0][2,2] 

ITERATION ismootherred : u [l] = ( ( I - ( ( (0.8) * ( inverse(diag(Laplacian [l])) ) ) *Laplacian [l] ) ) *u [l] ) + ( ( (0.8) * ( inverse(diag(Laplacian [l])) ) ) *f [l] ) order sred 
ITERATION ismootherblack : u [l] = ( ( I - ( ( (0.8) * ( inverse(diag(Laplacian [l])) ) ) *Laplacian [l] ) ) *u [l] ) + ( ( (0.8) * ( inverse(diag(Laplacian [l])) ) ) *f [l] ) order sblack 
ITERATION Jacobi : (ismootherred [l]) ~ (ismootherblack [l] ) 
ITERATION iprolong : u [l] = u [l] + ( ( transp (R) ) * u [lc]) order s 
ITERATION iresidual : res [l] = f [l] - (Laplacian [l] * u [l]) order s 
ITERATION irestrict : f [lc] = (R * res [l]) order s 
ITERATION icycle : if (l == 1) then (Jacobi [l])^8 
                  if (l != 1) then ( ( ((Jacobi [l])^1 ) ~ (iresidual [l]) ) ~ ( (irestrict [l]) ~ ( (icycle [lc])^1 ) ) ) ~ ( (iprolong [l]) ~ ( (Jacobi [l])^1 ) )

