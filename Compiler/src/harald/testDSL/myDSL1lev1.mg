Domain dom = [0,1] x [0,2]

FT f : dom^1 -> R^1
FT u : dom^1 -> R^1
OT Laplacian : ( dom^1 -> R^1 ) -> ( dom^1 -> R^1 )
OP Laplacian <0> = 'dx2' + 'dy2'

EQ pde : Laplacian [ u ] = f in dom
EQ bc : u = 0 in ddom
Accuracy = 4
Generate = 0
