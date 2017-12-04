# Template



### Template
brief

#### Syntax
<pre>
definition of syntax
</pre>

#### Details
more details; explanation of parameters if required

#### Example
<pre>
examples of usage
</pre>



# Basic Language Constructs



## Comments
Comments are offered with C-like syntax.

#### Syntax
<pre>
// <i>comment</i>
/* <i>comment</i> */
</pre>

#### Details


#### Example
<pre>
// this is a comment
</pre>
<pre>
/* this is a
   multiline
   comment
*/
</pre>




## Data types
Represents a simple data type.

#### Syntax
<pre>
<i>dataType</i>
</pre>

#### Details
*dataType* may be one of the following:
* Unit (must not be used in declarations)
* Real
* Integer or Int
* String
* Boolean or Bool
* a [higher dimensional data type](#higher-dimensional-data-types)

#### Example
cf examples for [variable declarations](#variable-declaration) and [function declarations](#function-declaration)



## Levels



### Level Access
Access to a single level.

#### Syntax
<pre>
@<i>level</i>
</pre>

#### Details
*level* may be on of the following
* a constant
* a [relative level](#relative-level)
* a suitable [level alias](#level-alias) evaluating to a single level

#### Example
<pre>@10</pre>



### Level Declaration
A group of levels used for declarations.

#### Syntax
<pre>
@<i>levels</i>
</pre>

#### Details
*levels* may be on of the following
* a constant
* a [relative level](#relative-level)
* a [level list](#level-list)
* a suitable [level alias](#level-alias)

#### Example
<pre>@10</pre>



### Relative Level
Access to a level relative to another.

#### Syntax
<pre>(<i>base</i> + <i>offset</i>)</pre>
<pre>(<i>base</i> - <i>offset</i>)</pre>

#### Details
*base* must be a constant or a suitable [level alias](#level-alias) evaluating to a single level.
*offset* must be an integer constant

#### Example
<pre>
(finest - 1)
</pre>



### Level Alias
Aliases for commonly used cases.

#### Syntax
<pre>
<i>alias</i>
</pre>

#### Details
*alias* may be on of the following:
Used as [level access](#level-access):
* current, denoting that the level of the surrounding construct should be used
* finer, shorthand for @(current + 1)
* coarser, shorthand for @(current - 1)

used as [level access](#level-access) and [level declaration](#level-declaration):
* finest, denoting the finest level as specified by Knowledge.maxLevel
* coarest, denoting the coarsest level as specified by Knowledge.minLevel

used as [level declaration](#level-declaration):
* all, denoting all levels as specified by the range between Knowledge.minLevel and Knowledge.maxLevel

#### Example
<pre>@all</pre>



### Level List
A list of levels used for [level declarations](#level-declaration).

#### Syntax
<pre>
(
  <i>levels</i>
  /* optionally */ but <i>exclude</i>
  )
</pre>

#### Details
*levels* must be a non-empty list of levels separated by comma or the and keyword, or a [level range](#level-range).
*exclude*, if provided, must be a valid [level list](#level-list) without any nested *exclude*. The not keyword may be used instead of the but keyword.

#### Example
<pre>@(0, 2, 4)</pre>
<pre>@(all but finest)</pre>



### Level Range
A range of levels used for [level declarations](#level-declaration).

#### Syntax
<pre>
( <i>begin</i> to <i>end</i> )
</pre>

#### Details
*begin* and *end* must be constants or suitable [level aliases](#level-alias) evaluating to single levels.

#### Example
<pre>@(coarsest to finest)</pre>



## Functions



### Function Declaration

Declares a new function with the given *name*, taking the provided *arguments* and with the given *returnType*.

#### Syntax
<pre>
/* optional */ noinline
  Function <i>name</i>
  /* optional */ <i>levels</i>
  /* optional */ ( <i>arguments</i> )
  /* optional */ : <i>returnType</i>
  { <i>body</i> }
</pre>

#### Details
Func may be used instead of Function.
noinline may be used to disallow inling for this function.
The function declaration is regarded as leveled if *levels* is specified. Must be a valid [level selection for declarations](#level-declaration).
*arguments*, if provided, has to be a list of [function arguments](#function-argument). May be empty.
*returnType*, if provided, has to be a valid [language datatype](#data-types). In this case, at least one [return statement](#return-statement) must be present in the *body*. If *returnType* is not provided, an implicit Unit type is assumed.

#### Example
<pre>
Function F ( xPos : Real, yPos : Real ) : Real {
  return sin ( xPos ) * cos ( yPos )
}
</pre>
<pre>
Function Smoother@all {
  /* ... */
}
</pre>



### Function Argument
Specifies a function argument with the given *name* and *dataType*.

#### Syntax
<pre>
<i>name</i> : <i>dataType</i>
</pre>

#### Details
*dataType* must be a valid [language datatype](#data-types).

#### Example
<pre>
someParam : Real
</pre>



### Return Statement
Statement used to exit a function and to optionally return a value.

#### Syntax
<pre>
return
  /* optionally */ <i>returnValue</i>
</pre>

#### Details
*returnValue* may be an arbitray expression. Its data type must match the surrounding functions return type. In case of Unit *returnValue* must not be specified.

#### Example
<pre>
return
</pre>
<pre>
return sin ( xPos ) * cos ( yPos )
</pre>



### Function Call

Call a function with the given *name* and the provided *parameters*.

#### Syntax
<pre>
<i>name</i>
  /* optional */ <i>levels</i>
  /* optional */ <i>offset</i>
  ( <i>parameters</i> )
</pre>

#### Details
If the called function is leveled and *levels* is not provided, an implicit [@current](#current) is assumed. *levels*, if provided, must be a valid [level access](#level-access).
*parameter* is a list of arbitrary expressions. May be empty.
If the function call is used as a statement, it is implicitly wrapped in an expression statement.

#### Example
<pre>
Var res : Real = F ( 0.5, 0.5 )
</pre>
<pre>
Smoother@current ( )
</pre>




## Local Declarations



### Variable Declaration
Declares a new variable

#### Syntax
<pre>
Var <i>name</i> : <i>dataType</i>
  /* optional */ levels
  /* optional */ = <i>initial</i>
</pre>

#### Details
Variable may be used instead of Var.
*dataType* must be a valid [language datatype](#data-types).
The declaration is regarded as leveled if *levels* is specified. Must be a valid [level selection for declarations](#level-declaration).
*initial* may be an arbitrary expression. It is used to initialize the variable in the generated code if provided.

#### Example
<pre>
Var cnt : Int = 0
Var curError : Real = 0
</pre>



### Value Declaration
Declares a new constant variable

#### Syntax
<pre>
Val <i>name</i> : <i>dataType</i>
  /* optional */ levels
  = <i>initial</i>
</pre>

#### Details
Value may be used instead of Val.
*dataType* must be a valid [language datatype](#datatype).
The declaration is regarded as leveled if *levels* is specified. Must be a valid [level selection for declarations](#level-declaration).
*initial* may be an arbitrary expression.
**Right now values are propagated by default and without check for side efffects. This effectively means that they work like a C define.** Can be controlled via Knowledge.experimental_l4_inlineValueDeclarations

#### Example
<pre>
Val maxNumIts : Int = 128
Val a : Real = 10.5 * 4.
</pre>



## Basic Loops



### Fixed-Length Loop
Executes statements a constant number of times.

#### Syntax
<pre>
repeat <i>n</i> times
  /* optional */ count <i>variable</i>
{ <i>body</i> }
</pre>

#### Details
*n* must be an integral number. *body* must be a non-empty list of statements.
The current iteration number is stored in *variable*, if provided, which needs to be declared beforehand.

#### Example
<pre>
Var cnt : Int = 0
repeat 10 times count cnt {
  print ( 'Iteration', cnt )
}
</pre>



### Conditional Loop
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



## Conditional
brief

#### Syntax
<pre>
if ( <i>condition</i> ) { ifBranch }
  /* optionally */ else <i>elseBranch</i>
</pre>

#### Details
Exectues *ifBranch* if *condition* evaluates to true, *elseBranch* otherwise (if it is specified).
*condition* may be an arbitrart expression evaluating to a Boolean.
*ifBranch* may be any list of statements or nothing.
*elseBranch* may be
* another [conditional](#conditional)
* a list of statements surrounded by curly brackets; the list may be empty

#### Example
<pre>
if ( true ) {
  print ( 'always true' )
} else {
  print ( 'will not be evaluated' )
}
</pre>
<pre>
Var someVar : Real = 0.5
if ( 0. == someVar or 1. == someVar ) {
  // ...
}



# DONE

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
   * *Runtime* Invert only at run-time of the program time
* `transpose()` to transpose a matrix
* `dot()` and `cross()` to calculate dot or cross product of two matrices
* `det()` to calculate the determinant
* supported operators for binary expression: `+`, `-`, `*`, `/`
* supported element-wise operations: `.*`, `*./`, `.%` (modulo), `.^` (power)



# IN PROGRESS

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



# TODO

DS features
Features from other languages
expression/statement
breakStatement
assignment
Operatorassignment TODO: assignment vs compound assignment
binop
Literals
Language extensions

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
