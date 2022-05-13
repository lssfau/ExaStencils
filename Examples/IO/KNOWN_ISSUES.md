# Parallel I/O

* Unable to open file: Mostly, this error occurs if a file is opened from a non-existing directory

# Visualization with ParaView or VisIt

## Xdmf 

### ParaView: Reader
  
ParaView offers 3 different readers for Xdmf files:
1. XDMF Reader
2. Xdmf3ReaderS
3. Xdmf3ReaderT

we found that the first option is the most robust and reliable

### ParaView and VisIt: Uniform meshes 

for the `Origin_DxDyDz` values of a `3DCoRectMesh` we need to know whether we create the files for ParaView or VisIt
since each reader interprets the order of coordinates differently (Paraview expects: Z, Y, X & VisIt expects: X, Y, Z)
(see https://gitlab.kitware.com/paraview/paraview/-/issues/13274)

for this purpose, use the knowledge flag `parIO_vis_generateVisItFiles`
	
### ParaView: vector plots (2D)

It might occur that an (often cellular) 2D vector quantity cannot be visualized correctly with ParaView. 
In this case, please use VisIt instead. VisIt most likely uses a different Xdmf reader.
Moreso, in case, that an Xdmf file cannot be visualized with VisIt, please try to use ParaView instead.
Note, that for non-uniform meshes the Xdmf files are equal for VisIt/ParaView and therefore do not need to be recreated.