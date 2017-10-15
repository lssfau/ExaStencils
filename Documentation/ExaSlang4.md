
### template
brief

#### Syntax
<pre>
</pre>

#### Details

#### Example
<pre>
</pre>


### LevelScope
Conditional execution of statements depending on the surrounding scope's level.

#### Syntax
<pre>
@<i>levels</i> { <i>body</i> }
</pre>

#### Details
*body* is only executed if the current level (usually given by a surrounding function) matches the given *levels*, or, in case of a given range or list, is included in it.

#### Example
<pre>
Function recursiveFct@all () {
  // do work

  @(all but coarsest) {
    recursiveFct@coarser ( )
  }
}
</pre>

## basic loops

### fixed-length loop
Executes statements a constant number of times.

#### Syntax
<pre>
repeat <i>n</i> times
  /* optional */ count <i>variable</i>
{ <i>body</i> }
</pre>

#### Details
*n* must be an integral number. *body* must be a non-empty list of statements.
`count variable`: the current iteration number is stored in *variable*, which needs to be declared beforehand.

#### Example
<pre>
Var cnt : Int = 0
repeat 10 times count cnt {
  print ( 'Iteration', cnt )
}
</pre>

### conditional loops
Executes statements while or until a given condition is met.

#### Syntax
<pre> repeat until <i>condition</i> { body } </pre>
<pre> repeat while <i>condition</i> { body } </pre>

#### Details

#### Example
<pre>
Var cnt : Int = 0
repeat until 10 == cnt {
  print ( 'Iteration', cnt )
  cnt += 1
}
</pre>



### communicate target

#### Syntax
<pre>
<i>target</i>
  /* optional */ <i>start</i> 
  /* optional */ to <i>end</i> // inclusive indices
</pre>

#### Details
*target* can be one of the following:
* all
* dup
* ghost

dup corresponds to duplicate layers. ghost corresponds to ghost layers. all corresponds to ghost and duplicate layers.

*start* and *end* have to be constant indices. Both are inclusive. If defined, the range of applicable layers to be communicated is restricted to the given range.

#### Example



### pre- and postComm
brief

#### Syntax
<pre>
preComm
  /* optional */ <i>op</i>
  <i>communicateTarget</i>  
  of <i>field</i>
  /* optional */ where <i>condition</i>
</pre>
<pre>
postComm
  /* optional */ <i>op</i>
  <i>communicateTarget</i>  
  of <i>field</i>
  /* optional */ where <i>condition</i>
</pre>

#### Details

*op* can be one of the following:
* begin
* finish

If *op* is not given, a synchronous (w.r.t. the Layer 4 execution path) communication is performed.

*communicateTarget* is specified by [communicate target](#communicate-target)

*condition* can be used to restrict the communication to points fulfilling it. **not fully implement yet** 

#### Example
<pre>
precomm all of Solution@current
postcomm dup of Solution where 0 == ( i0 + i1 ) % 2
</pre>



### loop over fields
Loops over a given field and executes statements at each point.

#### Syntax
<pre>
loop over <i>field</i>
  /* optional */ only <i>region</i>
  /* optional */ sequentially
  /* optional */ where <i>condition</i>
  /* optional */ starting <i>offsetBegin</i>
  /* optional */ ending <i>offsetEnd</i>
  /* optional */ stepping <i>stepSize</i>
  /* optional */ with <i>reduction</i>
  /* optional */ <i>preComm</i>
  /* optional */ <i>postComm</i>
{ <i>body</i> }
</pre>

preComm and postComm can be one or more expressions in the form of [pre- and postcomm](#pre-and-postcomm)

#### Details


#### Example
<pre>
</pre>




DS features
Levels and leveled objects
Features from other languages
expression/statement
variableDeclaration
valueDeclaration
Basic data types
Loops
breakStatement
assignment
Operatorassignment TODO: assignment vs compound assignment
Functions and functionCall
returnStatement
conditional
binop
Literals
Language extensions

## Higher-Dimensional Data Types
### Vectors
Represents a one-dimensional number of scalar elements

#### Syntax
<pre>
{<i>expression</i>, <i>expression</i>, ...  } /* optional */ T
</pre>

#### Details
At least one *expression* is required to construct a one-dimensional vector.
*T* can be used to transpose the defined vector expression.
#### Example
<pre>
Var v1 : Vector&lt;Real, 3&gt; = { 1, 2, 3}
Var v2 : Vec3 = [1 2 3] + [4 5 6]
Var v3 : ColumnVector&lt;Int, 3&gt; = {1, 2, 3}T
</pre>
#### built-in functions
* `transpose()` to transpose a vector. For anonymous vector definitions, a suffixing `T` can be used.
* `dot()` to calculate dot product of two vectors
* supported operators for binary expression: `+`, `-`, `*`, `/`
* supported element-wise operations: `.*`, `*./`, `.%` (modulo), `.^` (power)

### Matrices
Represents a two-dimensional number of scalar elements

#### Syntax
<pre>
{{<i>expression</i>, <i>expression</i>, ...  }, {<i>expression</i>, <i>expression</i>, ...  }, ... }
</pre>

#### Details
At least one *expression* is required to construct a 1x1 matrix.
#### Example
<pre>
Var m1 : Matrix&lt;Real, 2, 2&gt; = {{1, 2}, {3, 4}}
Var m2 : Matrix&lt;Real, 2, 3&gt; = [1 2 3; 4 5 6]
</pre>
#### built-in functions
* `inverse()` invert a matrix
   Matrices up to 3x3 are inverted directly at generation time. For larger matrices, a strategy can be selected via the Knowledge parameter`experimental_resolveInverseFunctionCall`:
   * *Cofactors*: Invert at generation-time using cofactors matrix
   * *GaussJordan*: Invert at generation-time using the Gauss-Jordan algorithm
   *  *Runtime* Invert only at run-time of the program time
* `transpose()` to transpose a matrix
* `dot()` and `cross()` to calculate dot or cross product of two matrices
* `det()` to calculate the determinant
* supported operators for binary expression: `+`, `-`, `*`, `/`
* supported element-wise operations: `.*`, `*./`, `.%` (modulo), `.^` (power)




Globals
Index
Function templates & instantiation
Domains
Fields and layouts
Apply bc
Slots
advanceStatement
Operators
Stencils
Offset notation
Mapping notation
Diag()
inverse()
stencilFields
Advanced domain specific features
unresolvedAccess
contractionLoop
loopOver
fieldIteratorAccess
Reduction
...
loopOverFragments
applyBCsStatement
communicateStatement
solveLocallyStatement
colorWithStatement
parallelization/partitioning
communicate

