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



# Language Structure



## Top-Level Statements
The following statements are allowed:
* [import](#import)
* [inline knowledge](#inline-knowledge)
* declarations of knowledge objects
    * [domain declaration](#domain-declaration)
    * [field layout declaration](#field-layout-declaration)
    * [field declaration](#field-declaration)
    * [external field declaration](#external-field-declaration)
    * [direct stencil declaration](#direct-stencil-declaration)
    * [stencil declaration from expression](#stencil-from-expression)
    * [stencil declaration from default](#stencil-from-default)
* [stencil field declaration](#stencil-field-declaration)
* [global section](#global-section)
* function declarations
    * [function declaration](#function-declaration)
    * [function template declaration](#function-template)
    * [function instantiation](#function-instantiation)



## Inner Statements
The following statements are allowed:
* local declartions
    * [variable declaration](#variable-declaration)
    * [value declaration](#value-declaration)
* [assignment](#basic-assignment) and [compound assignment](#compound-assignment)
* loops
    * [fixed-length loop](#fixed-length-loop)
    * [conditional loop](#conditional-loop)
    * [contraction loop](#contraction-loop)
    * [field loop](#field-loop)
    * [fragment loop](#fragment-loop)
* [conditional](#conditional)
* [function call](#function-call); implicitly wrapped in an expression statement
* [apply bc](#apply-bc)
* [communicate](#communicate)
* [advance](#advance)
* [return statement](#return-statement)
* [level scope](#level-scope)
* [local solve](#local-solve)
* [color statement](#color-statement)



## Concepts
TODO


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
cf examples for [variable declarations](#variable-declaration) and [function declarations](#function-declaration).



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
<pre>@(finest - 1)</pre>



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

Used as [level access](#level-access) and [level declaration](#level-declaration):
* finest, denoting the finest level as specified by Knowledge.maxLevel
* coarest, denoting the coarsest level as specified by Knowledge.minLevel

Used as [level declaration](#level-declaration):
* all, denoting all levels as specified by the range between Knowledge.minLevel and Knowledge.maxLevel

#### Example
<pre>@all</pre>



### Level List
A list of levels used for [level declarations](#level-declaration).

#### Syntax
<pre>
(
  <i>levels</i>
  /* optional */ but <i>exclude</i>
  )
</pre>

#### Details
*levels* must be a non-empty list of levels separated by comma or the and keyword, or a [level range](#level-range).
*exclude*, if provided, must be a valid [level list](#level-list) without any nested *exclude*. The not keyword may be used instead of the but keyword.

#### Example
<pre>(0, 2, 4)</pre>
<pre>(all but finest)</pre>



### Level Range
A range of levels used for [level declarations](#level-declaration).

#### Syntax
<pre>
( <i>begin</i> to <i>end</i> )
</pre>

#### Details
*begin* and *end* must be constants or suitable [level aliases](#level-alias) evaluating to single levels.

#### Example
<pre>(coarsest to finest)</pre>



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
*body* must be a list of suitable [statements](#inner-statements). May be empty.

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
  /* optional */ <i>returnValue</i>
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



## Assignments



### Basic Assignment
Assigns *src* to *dest*.

#### Syntax
<pre>
<i>dest</i> = <i>src</i>
</pre>

#### Details
*dest* must be a valid access.
*src* maybe an arbitrary expression evaluating to the data type of *dest*.

#### Example
<pre>
Var cnt : Int
cnt = 0
</pre>



### Compound Assignment
Performs a compound assignment from *src* to *dest* with the specified operation.

#### Syntax
<pre><i>dest</i> += <i>src</i></pre>
<pre><i>dest</i> -= <i>src</i></pre>
<pre><i>dest</i> *= <i>src</i></pre>
<pre><i>dest</i> /= <i>src</i></pre>

#### Details
*dest* must be a valid access.
*src* maybe an arbitrary expression evaluating to the data type of *dest*.

#### Example
<pre>
Var cnt : Int = 0
cnt += 1
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
*n* must be an integral number.
The current iteration number is stored in *variable*, if provided, which needs to be declared beforehand.
*body* must be a list of suitable [statements](#inner-statements). May be empty.

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
*condition* may be an arbitrary expression evaluating to a Boolean value.
*body* must be a list of suitable [statements](#inner-statements). May be empty.

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
  /* optional */ else <i>elseBranch</i>
</pre>

#### Details
Exectues *ifBranch* if *condition* evaluates to true, *elseBranch* otherwise (if it is specified).
*condition* may be an arbitrart expression evaluating to a Boolean.
*ifBranch* must be a list of suitable [statements](#inner-statements). May be empty.
*elseBranch* may be
* another [conditional](#conditional)
* a list of suitable [statements](#inner-statements); may be empty

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
</pre>



# Knowledge Objects



## Domains



### Domain Declaration
Declares a new domain with the given *name*.

#### Syntax
<pre>
Domain <i>name</i> &lt; <i>lower</i> to <i>upper</i> &gt;
</pre>

#### Details
*lower* and *upper* are coordinates of the axis-aligned bounding box surrounding the domain. Their dimensionalty must match. **Currently it must also be identical to Knowledge.dimesionality**.
In case of multiple domains, the boundaries of all (sub-)domains must coincide with the doamin partitioning.
There must be at least one domain named 'global'. This domain must include all other domains.

#### Example
<pre>Domain unitSquare &lt; [0., 0.] to [1., 1.] &gt;</pre>
<pre>Domain global &lt; [0, 0, 0] to [2, 4, 6] &gt;</pre>



## Fields



### Field Layout Declaration
Declares a new field layout with the given *name* and *options*.

#### Syntax
<pre>
Layout <i>name</i> &lt; <i>dataType</i> , <i>localization</i> &gt; 
  /* optional */ <i>levels</i>
  { <i>options</i> }
</pre>

#### Details
Specifies that quatities of the given *dataType* are stored at specific parts of the grid chosen by *localization*.
*dataType* must be a valid [language datatype](#data-types).
*localization* must be one of the following:
* Node
* Cell
* Face_x
* Face_y if dimensionality at least 2
* Face_z if dimensionality at least 3

The declaration is always regarded as leveled. If *levels* is specified it must be a valid [level selection for declarations](#level-declaration). If it is not specified, an implicit [@all](#level-declaration) is assumed.
*layoutOptions* is a list of [layout options](#layout-option) which may be separated by comma. May be empty.

#### Example
<pre>
Layout CellLayout &lt; Real , Cell &gt; @all {
  duplicateLayers = [ 0, 0, 0 ]
  ghostLayers     = [ 1, 1, 1 ] with communication
}
</pre>



### Layout Option
Specification of an option to be used for [field layouts](#field-layout-declaration).

#### Syntax
<pre>
<i>option</i> = <i>index</i>
  /* optional */ with communication
</pre>

#### Details
*option* may be one of the following:
* duplicateLayers
* ghostLayers
* innerPoints

*index* specifies the number of layers per dimension for the chosen option.
with communication marks the chosen layers for communication. **Layers that are not marked here will not be communicated, even when communicate statements are given**.

#### Example
cf example for [field layout declarations](#field-layout-declaration)




### Field Declaration
Declares a new field with the given *name* and the provided options.

#### Syntax
<pre>
Field <i>name</i> &lt; <i>domain</i> , <i>layout</i> , <i>boundaryCondition</i> &gt;
  /* optional */ [ <i>numSlots</i> ]
  /* optional */ <i>levels</i>
</pre>

#### Details
Each field is tied to a specific *domain* accessed through its name.
Data type, localization of the field, etc is controlled by the linked *layout*. An implicit [@current](#level-alias) is always assumed. **This is only a name - providing a level in addition is not supported**.
*boundaryCondition* may be a valid [boundary condition](#boundary condition).
If *numSlots* is specified, the field is slotted with the given number. Must be an integer constant.
The declaration is always regarded as leveled. If *levels* is specified it must be a valid [level selection for declarations](#level-declaration). If it is not specified, an implicit [@all](#level-declaration) is assumed.

#### Example
<pre>Field vis &lt; global, CellLayout, None &gt;</pre>
<pre>Field rho &lt; global, CellLayout, Neumann &gt; [2] @all</pre>



### Boundary Condition
Specifies the boundary conditions to be used for a given [field](#field-declaration)

#### Syntax
<pre>None</pre>
<pre>
Neumann
  /* optional */ ( <i>order</i> )
</pre>
<pre><i>dirichlet</i></pre>
<pre><i>bcFunction</i> ( )</pre>

#### Details
None corresponds to no boundary handling.
Neumann corresponds to Neumann-0 boundary conditions. If *order* is not specified it is defaulted to Knowledge.discr_defaultNeumannOrder.
*dirichlet* may take the shape of an arbirtrary expression evaluating to the data type of the field. 
*bcFunction* is a function reference, with optional level, to a user function implementing the boundary handling routine. The function's return type must be Unit.

**Since *dirchlet* can be a function call, an thus may look like a call to a *bcFunction*, the return type of the called function must be known at generation time. It is used to switch both cases.**

#### Example
<pre>Neumann ( 1 )</pre>
<pre>sin ( vf_boundaryPosition_x )</pre>
<pre>applyBoundaries ( )</pre>



### External Field Declaration
Declares a new external field with the given *name* and the provided options.

#### Syntax
<pre>
external Field <i>name</i> &lt; <i>layout</i> &gt; =&gt; <i>internal</i>
</pre>

#### Details
Used to transfer data from external program parts to fields declared in the DSL.
*layout* must describe the layout of the external counterpart.
*internal* is a refernece to a declared field. A level specification is necessary.
Using this information, copy-in and copy-out functions are set up. These can be called from the external program.
If Knowledge.generateFortranInterface is set, the external counterpart is assumed to be in Fortran-compilant ordering, otherwise lexicographic ordering is assumed.

#### Example
<pre>
Layout CellLayout &lt; Real , Cell &gt; { /* ... */ }
Layout ExternalLayout &lt; Real , Cell &gt; @finest { /* ... */ }

Field p &lt; global, CellLayout, Neumann &gt;

external Field p_0 &lt; ExternalLayout &gt; =&gt; p@finest
</pre>



## Stencils



### Direct Stencil Declaration
Declares a new stencil with the given *name* and the provided *entries*.

#### Syntax
<pre>
Stencil <i>name</i>
  /* optional */ <i>levels</i>
  { <i>entries</i> }
</pre>

#### Details
The declaration is always regarded as leveled. If *levels* is specified it must be a valid [level selection for declarations](#level-declaration). If it is not specified, an implicit [@all](#level-declaration) is assumed.
*entries* is a list of [offset entries](#stencil-offset-entry) or [mapping entries](#stencil-mapping-entry). May be separated by comma. May be empty.

#### Example
<pre>
Stencil FivePoint@all {
  [ 0,  0] =&gt;  4.0
  [-1,  0] =&gt; -1.0
  [ 1,  0] =&gt; -1.0
  [ 0, -1] =&gt; -1.0
  [ 0,  1] =&gt; -1.0
}
</pre>
<pre>
Stencil RestrictCell {
  [i0, i1] from [ 2 * i0,     2 * i1     ] with 0.25
  [i0, i1] from [ 2 * i0,     2 * i1 + 1 ] with 0.25
  [i0, i1] from [ 2 * i0 + 1, 2 * i1     ] with 0.25
  [i0, i1] from [ 2 * i0 + 1, 2 * i1 + 1 ] with 0.25
}
</pre>



### Stencil Offset Entry
A single [stencil](#direct-stencil-declaration) entry in offset notation.

#### Syntax
<pre>
<i>offset</i> =&gt; <i>coefficient</i>
</pre>

#### Details
*offset* must be a const index.
*coefficient* may be an arbitrary expression.

#### Example
<pre>
[ 0,  0] =&gt;  4.0 * alpha + epsilon
</pre>



### Stencil Mapping Entry
A single [stencil](#direct-stencil-declaration) entry in mapping notation.

#### Syntax
<pre>
<i>row</i> from <i>col</i> with <i>coefficient</i>
</pre>

#### Details
*row* and *col* can be interpreted as the corresponding row and column positions for the coefficient were the matrix represented by the stencil constructed explicitly.
*coefficient* may be an arbitrary expression.

#### Example
<pre>
[i0, i1] from [ 2 * i0, 2 * i1 ] with 1.0
</pre>



### Stencil From Expression
Declares a new stencil with the given *name* and constructs it based on *expression*

#### Syntax
<pre>
Stencil <i>name</i>
  /* optional */ <i>levels</i>
  from <i>expression</i>
</pre>

#### Details
*expression* may be an arbitrary expression evaluating to a stencil.
Supported operations on stencils are:
* scaling
* addition
* multiplication
* transpose
* kron (Kronecker product)

#### Example
<pre>
Stencil Horizontal { /* ... */ }
Stencil Vertical   { /* ... */ }
Stencil Combined from 2.0 * ( Horizontal + Vertical )
</pre>



### Stencil From Default
Declares a new stencil with the given *name* and constructs it based on the specified default *operation*.

#### Syntax
<pre>
Stencil <i>name</i>
  /* optional */ <i>levels</i>
  from default <i>operation</i> on <i>localization</i> with <i>parameter</i>
</pre>

#### Details
*operation* may be either restriction or prolongation.
*localization* specifies where the operator will be applied. Allowed values are the same as for [field layout declarations](#field-layout-declaration).
For restriction and prolongation, *parameter* specifies the interpolation scheme. It may be 'linear' for values of discretized functions and 'integral_linear' for values of integrals over discretized functions. The former is usually applied in finite difference contexts, while the latter finds application in finite volume contexts.

#### Example
<pre>
RestrictNode from default restriction on Node with 'linear'
</pre>



### Stencil Field Declaration
Declares a new stencil field with the given *name*, using the shape of *stencil* and storing the data in *field*.

#### Syntax
<pre>
StencilField <i>name</i> &lt; field =&gt; stencil &gt;
  /* optional */ <i>levels</i>
</pre>

#### Details
For *stencil* and *field*, an implicit [@current](#level-alias) is always assumed. **This is only a name - providing a level in addition is not supported**.
The declaration is always regarded as leveled. If *levels* is specified it must be a valid [level selection for declarations](#level-declaration). If it is not specified, an implicit [@all](#level-declaration) is assumed.
The order of coefficients in the linked *stencil* will remain and be mapped to the entries of the linked *field*'s data.
The data type of the linked *field* has to be vector. The vector size must match the number of coefficients in the linked *stencil*.
The coefficients of the stencil are used to initialize the field's data.

#### Example
<pre>
Layout SfLayout &lt; Vector &lt; Real, 5 &gt; , Cell &gt; { /* ... */ }
Field StencilData &lt; global, SfLayout, None &gt;

Stencil FivePointShape { /* ... */ }

StencilField StoredFivePoint &lt; StencilData =&gt; FivePointShape &gt;
</pre>



# Domain-Specific Features



## Special Loops



### Field Loop
Loops over the given *field* and executes the *body* at each point.

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

#### Details
If no level is provided for *field*, an implicit [@current](#level-alias) is assumed. Other modifiers such as slot, offset or direction access are ignored.

only *region* may be used to restrict iteration to a specific [region](#region). If it is not provided, inner is assumed.

sequentially prevents shared memory parallelization. **This is only a temporary workaround**.

where *condition* may be used to restrict the execution of *body* to only cases where condition is fullfilled. Prominent applications are [colored kernels](#color-with).

*offsetBegin* and *offsetEnd* may be used to extend or restrict the iteration space. [Constant indices](#constant-index) are required. *offsetBegin* is added to the loop start while *offsetEnd* is subtracted from the loop end.

*stepSize* may be used to adapt the geneated loops' step size. It must be a positive, non-zero [constant indices](#constant-index).

*reduction* must be a valid [reduction](#reduction).

*preComm* and *postComm*, if defined, must be one or more expressions in the form of [pre- and postcomm](#pre-and-postcomm).

*body* must be a list of suitable [statements](#inner-statements). May be empty.

If a field loop is not in the scope of a [fragment loop](#fragment-loop), an implicit [fragment loop](#fragment-loop) is wrapped around the field loop in the generation processs.

#### Example
<pre>loop over u { /* ... */ }</pre>
<pre>loop over u where 0 == ( i0 + i1 ) % 2 { /* ... */ }</pre>
<pre>loop over u only ghost [0, -1] on boundary { /* ... */ }</pre>
<pre>loop over u starting [1, 0] stepping [2, 1] { /* ... */ }</pre>
<pre>
Var curErr : Real = 0.0
loop over u with reduction ( max, curErr ) { /* ... */ }
</pre>



### Region
Specifies an iteration region for a [field loop](#field-loop).

#### Syntax
<pre>
<i>target</i> <i>direction</i>
  /* optional */ on boundary
</pre>

#### Details
*target* may be one of the following:
* inner
* dup
* ghost

dup corresponds to duplicate layers. ghost corresponds to ghost layers. inner corresponds to inner layers.

*direction* specifies which part of the current fragment's grid is selected. For example, [1, 0] would select the right edge. *direction* must be a [constant indices](#constant-index). In case of inner as *target*, *direction* is ignored but should be zero in all dimensions by convention.

If on boundary is specified, the loop is only executed at outer boundaries, i.e., for fragments that don't have a neighbor in the specified *direction*.

#### Example
<pre>
only dup [0, -1] on boundary
</pre>



### Fragment Loop
Loops over all fragments.

#### Syntax
<pre>
loop over fragments
  /* optional */ with <i>reduction</i>
  { <i>body</i> }
</pre>

#### Details
*reduction* must be a valid [reduction](#reduction).

*body* must be a list of suitable [statements](#inner-statements). May be empty.

*body* usually contains at least one [field loop](#field-loop). [Communicate statements](#communicate-statement) and [apply bc statements](apply-bc-statement) inside fragment loops are supported.

#### Example
<pre>
Var sum : Real = 0.0
loop over fragments with reduction ( + : sum ) {
  loop over u with reduction ( + : sum ) { /* ... */ }
}
</pre>



### Reduction
Specifies a reduction to be performed for an associated [special loop](#special-loops).

#### Syntax
<pre>
reduction ( <i>op</i> : <i>target</i> )
</pre>

#### Details
*op* may be one of the following:
* \+ or \*
* min or max

*target* must be the name of a previously declared variable.

If Knowledge.experimental_trimBoundsForReductionLoops is enabled, iteration spaces are adapted such that duplicate points are only handled on one fragment. This prevents adding the contibution of the same duplicate point mulitple times.

#### Example
<pre>
Var sum : Real = 0.0
loop over u with reduction ( + : sum ) { /* ... */ }
</pre>



### Contraction Loop
TODO: Stefan

#### Syntax
<pre>
repeat <i>n</i> times
  /* optional */ count <i>variable</i>
  with contraction posExtent
  /* optional */ , negExtent
  { <i>body</i> }
</pre>

#### Details
*n* must be an integral number.

The current iteration number is stored in *variable*, if provided, which needs to be declared beforehand.

*body* must be a list of suitable [statements](#inner-statements). May be empty.

TODO: Stefan

#### Example
<pre>
TODO: Stefan
</pre>



## Communication



### Communicate
Triggers data exchange for the given *field*.

#### Syntax
<pre>
/* optional */ begin
  /* optional */ finish
  communicate <i>targets</i> of <i>field</i>
  /* optional */ where <i>condition</i>
</pre>

#### Details
begin and finish are mutually exclusive. They trigger the start and end of a communication phase. The user is responsible for ensuring that matching pairs exist and are called accordingly. If neither is given, a full communication is performed.

communicating can be used instead of communicate.

*targets* must be a list of valid [communicate targets](#communicate-target). It may be empty in which case all is assumed.

*field* must be a valid reference to a field to be communicated. An implicit [@current](#level-alias) is assumed if no level specification is given. Slot modifiers are honored. Other modifiers such as offst and direction access are ignored.

*condition* can be used to restrict exchange to data points fulfilling the given *condition*. It may be an arbitrary expression evaluating to a Boolean. **NOT IMPLEMENTED YET**

#### Example
<pre>communicate u[next]@current</pre>
<pre>
begin communicating u
/* ... */
finish communicating u
</pre>



### Communicate Target
brief

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
<pre>
examples of usage
</pre>



### Pre- and PostComm
Specifies a communication step to be performed before or after an associated [loop](#field-loop).

#### Syntax
<pre>
preComm
  /* optional */ <i>op</i>
  <i>target</i>  
  of <i>field</i>
  /* optional */ where <i>condition</i>
</pre>
<pre>
postComm
  /* optional */ <i>op</i>
  <i>target</i>  
  of <i>field</i>
  /* optional */ where <i>condition</i>
</pre>

#### Details

*op* can be one of the following:
* begin
* finish

If *op* is not given, a synchronous (w.r.t. the Layer 4 execution path) communication is performed.

*target* must be a valid [communicate target](#communicate-target).

*condition* can be used to restrict the communication to points fulfilling it. **This function is not fully implement yet.**

#### Example
<pre>precomm all of Solution@current</pre>
<pre>postcomm dup of Solution where 0 == ( i0 + i1 ) % 2</pre>



## Field Operations



### Apply BC
Applies the boundary conditions of a given *field*.

#### Syntax
<pre>
apply bc to <i>field</i>
</pre>

#### Details
*field* must be a valid reference to a field to be updated. An implicit [@current](#level-alias) is assumed if no level specification is given. Slot modifiers are honored. Other modifiers such as offst and direction access are ignored.

#### Example
<pre>
apply bc to u
</pre>



### Advance
Advances the slot of a given *field*.

#### Syntax
<pre>
advance <i>field</i>
</pre>

#### Details
*field* must be a valid reference to a field. An implicit [@current](#level-alias) is assumed if no level specification is given. Slot modifiers and other modifiers such as offst and direction access are ignored.

#### Example
<pre>
advance u
</pre>



## Solver Extensions



### Local Solve
Solves for multiple unknowns at once.

#### Syntax
<pre>
solve locally
  /* optional */ with jacobi
  /* optional */ relax <i>omega</i>
  { <i>components</i> }
</pre>

#### Details

Internally, a system of equations based on the given *components* is construced and solved. The solution to the system is then written back to the unknowns.

with jacobi specifies that the writeback should be performed to the next slot of the unknowns' fields.

*omega*, if provided, will be used to relax the found solution to the local system. It may be an arbitrary expression that evaluated to Double.

*components* must be a list of of valid [solve components](#solve-component).

Must be inside the scope of a [field loop](#field-loop).

#### Example
<pre>
loop over u {
  solve locally relax 0.8 {
    u@[0, 0] =&gt; laplace@[0, 0] * u@[0, 0] == rhs_u@[0, 0]
    u@[1, 0] =&gt; laplace@[1, 0] * u@[1, 0] == rhs_u@[1, 0]
  }
}
</pre>



### Solve Component
Specifies one component of a [local solve block](#local-solve).

#### Syntax
<pre>
<i>unknown</i> =&gt; <i>lhs</i> == <i>rhs</i>
</pre>

#### Details
*unknown* must be a valid reference to a field. An implicit [@current](#level-alias) is assumed if no level specification is given. Other modifiers are honored.

*lhs* and *rhs* are the left- and right-hand sides of the equation to be solved. They may be arbitrary expressions evaluating to the same data type. Equations not including the *unknown* are supported.

#### Example
<pre>u =&gt; laplace * u == 0.0</pre>
<pre>u@[1, 0] =&gt; laplace@[1, 0] * u@[1, 0] == rhs_u@[1, 0]</pre>



### Color Statement
Colors a set of statements in a *body* with given *colors*.

#### Syntax
<pre>
color with { <i>colors</i> , <i>body</i> }
</pre>

#### Details
*colors* must be a comma-separated list of arbitrary expressions, each evaluating to Boolean.

*body* must be a list of suitable [statements](#inner-statements). May be empty. Currently only [field loops](#field-loop) are colored. Other statements remain untouched. In future versions, [apply bc](#apply-bc) and [communicate](#communicate) will be handled as well.

During unfolding, the *body* is duplicated once for each color. It is then adapted to the current color by adding the color expression to, e.g., [field loops](#field-loop) as (potentially additional) condition.

#### Example
<pre>
color with {
  ( 0 == ( i0 + i1 ) % 2 ),
  ( 1 == ( i0 + i1 ) % 2 ),
  
  loop over p { /* ... */ }
  communicate p
}
</pre>



# Language Extensions



## Top-Level



### Import
Imports the content of another DSL file.

#### Syntax
<pre>
import <i>filename</i>
</pre>

#### Details
*filename* is relative to the location of the current DSL file. The location is automatically adapted for nested imports.
Multiple import statements in the same file are supported.

#### Example
<pre>
import '../lib/defaultGlobals.exa4'
</pre>



### Inline Knowledge
Allows specifying knowledge parameters in the DSL.

#### Syntax
<pre>
Knowledge { <i>parameters</i> }
</pre>

#### Details
*parameters* must be a list of key-value assigments. Separation by comma is not supported. May be empty.
Multiple knowledge inlines in the same file are supported.

#### Example
<pre>
Knowledge {
  opt_useAddressPrecalc = true
  opt_unroll            = 2
}
</pre>



### Global Section
Opens a new global section holding declarations of global [variables](#variable-declaration) and [values](#value-declaration).

#### Syntax
<pre>
Globals { <i>entries</i> }
</pre>

#### Details
*entries* must be a list of valid [variable declarations](#variable-declaration) and [value declarations](#value-declaration). May be empty.
Multiple global sections in one DSL document are supported.

#### Example
<pre>
Globals {
  Var omega     : Real = 0.8
  Val maxNumIts : Int  = 128
}
</pre>



### Function Template
Declares a new function template with the given *name*.

#### Syntax
<pre>
FunctionTemplate <i>name</i> &lt; <i>templateParams</i> &gt; ( <i>functionParams</i> ) : <i>returnType</i> { <i>body</i> }
</pre>

#### Details
FuncTemplate can be used instead of FunctionTemplate.
*templateParams* must be a list of identifiers that must be separated by comma or newline. May be empty.
*functionParams* must be a list of [function arguments](#function-argument). May be separated by comma. May be empty.
*returnType* must be a valid [language data type](#data-types).
This statement will not map to a function without suitable [function instantiations](#function-instantiation).
*body* must be a list of suitable [statements](#inner-statements). May be empty.

#### Example
<pre>
FunctionTemplate SetFieldComponent &lt; target &gt; ( value : Real ) : Unit {
  loop over target {
    target = value
  }
}
</pre>



### Function Instantiation
Instantiates a [function template](#function-template) as a new function with the given *name*.

#### Syntax
<pre>
Instantiate <i>template</i> &lt; <i>templateArgs</i> &gt; as <i>name</i>
  /* optional */ <i>levels</i>
</pre>

#### Details
Inst can be used instead of Instantiate.
*template* must be the name of a declared [function template](#function-template).
*templateArgs* must be a list of expressions that must be separted by comma or newline. Its length must match the length on the template parameter list of the linked function template.
*name* is the name of the newly created function. If a suitable [level declaration](#level-declaration) is given, the new function inherits it.
In the instantion, occurences of each function template parameter in the function template body are replaced with the corresponding given argument. Modifiers such as level, offset or direction accesses are merged.

#### Example
<pre>
Instantiate SetFieldComponent &lt; u@current &gt; as SetSolution_u@all
Instantiate SetFieldComponent &lt; v@current &gt; as SetSolution_v@all
</pre>



### Layout Transformation
TODO: Stefan



## Inner



### LevelScope
Conditional execution of statements depending on the surrounding scope's level.

#### Syntax
<pre>
<i>levels</i> { <i>body</i> }
</pre>

#### Details
*body* is only executed if the current level (usually given by a surrounding function) matches the given *levels*, or, in case of a given range or list, is included in it.

*levels* must be a valid [level declaration](#level-declaration).

*body* must be a list of suitable [statements](#inner-statements). May be empty.

#### Example
<pre>
Function recursive@all () {
  /* ... */

  @(all but coarsest) {
    recursive@coarser ( )
  }
}
</pre>



# TO BE INTEGRATED



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



# TODO

expression/statement
binop
Literals

Index
Offset notation
Mapping notation
Diag()
inverse()
unresolvedAccess
fieldIteratorAccess
