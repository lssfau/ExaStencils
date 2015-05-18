package exastencils.util

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.prettyprinting._
import exastencils.knowledge.Knowledge

case class Matrix() extends Node with FilePrettyPrintable {
  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"Util/Matrix.h");

    writer.addExternalDependency("cmath")
    writer.addInternalDependency("Util/Vector.h")

    writer << """
//=====================================================================================================================
//                                        _    __   ____   ____     ______   ____
//                                       | |  / /  /  _/  / __ \   / ____/  / __ \
//                                       | | / /   / /   / /_/ /  / __/    / /_/ /
//                                       | |/ /  _/ /   / ____/  / /___   / _, _/
//                                       |___/  /___/  /_/      /_____/  /_/ |_|
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file Matrix.h
/// \brief  Header file for the Matrix classes
/// \author Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// forward declarations
//=====================================================================================================================

template <typename T> class TMatrix4x4;
template <typename T> class TMatrix3x3;

//=====================================================================================================================
// typedefs
//=====================================================================================================================
"""
    writer << "typedef TMatrix4x4<" + (if (Knowledge.useDblPrecision) "double" else "float") + "> Mat4;        ///< a 4x4 float matrix\n"
    writer << "typedef TMatrix3x3<" + (if (Knowledge.useDblPrecision) "double" else "float") + "> Mat3;       ///< a 3x3 float matrix\n"

    writer << """

//=====================================================================================================================
// class
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \class  TMatrix3x3
/// \brief  Represents a basic 3 by 3 square matrix
//
//=====================================================================================================================
template <typename T>
class TMatrix3x3
{
public:
  //=================================================================================================================
  // constructors / destructor
  //=================================================================================================================

  /// \brief    default constructor for the TMatrix3x3 class; initializes an identity matrix
  TMatrix3x3 ()
  {
    for (unsigned int i = 0; i < 3; ++i)
      for (unsigned int j = 0; j < 3; ++j)
        (*this)(i, j) = (i == j) ? (T)1 : (T)0;
  }

  /// \brief    copy constructor for the TMatrix3x3 class
  /// \param    other the source of the copy operation
  TMatrix3x3 (const TMatrix3x3<T>& other)
  { std::copy(other.m, other.m + 9, m); }

  /// \brief    constructor for the TMatrix3x3 class; initializes every element with the given value
  /// \param    value initial value for every matrix element
  explicit TMatrix3x3 (T value)
  {
    for (unsigned int i = 0; i < 3; ++i)
      for (unsigned int j = 0; j < 3; ++j)
        (*this)(i, j) = value;
  }

  /// \brief    constructor for the TMatrix3x3 class; copies the given data
  /// \param    mat   initial values for each matrix element
  explicit TMatrix3x3 (T mat[9])
  { std::copy(mat, mat + 9, m); }

  /// \brief    constructor for the TMatrix3x3 class; copies the given data
  /// \param    a00   element (0, 0) of the source matrix
  /// \param    a01   element (0, 1) of the source matrix
  /// \param    a02   element (0, 2) of the source matrix
  /// \param    a10   element (1, 0) of the source matrix
  /// \param    a11   element (1, 1) of the source matrix
  /// \param    a12   element (1, 2) of the source matrix
  /// \param    a20   element (2, 0) of the source matrix
  /// \param    a21   element (2, 1) of the source matrix
  /// \param    a22   element (2, 2) of the source matrix
  explicit TMatrix3x3 (T a00, T a01, T a02, T a10, T a11, T a12, T a20, T a21, T a22)
  {
    (*this)(0, 0) = a00;
    (*this)(0, 1) = a01;
    (*this)(0, 2) = a02;
    (*this)(1, 0) = a10;
    (*this)(1, 1) = a11;
    (*this)(1, 2) = a12;
    (*this)(2, 0) = a20;
    (*this)(2, 1) = a21;
    (*this)(2, 2) = a22;
  }

  /// \brief    default destructor for the TMatrix3x3 class
  ~TMatrix3x3 ()
  {}

  //=================================================================================================================
  // operators
  //=================================================================================================================

  /// \brief    operator giving element access
  /// \param    i row of the element to be accessed
  /// \param    j column of the element to be accessed
  /// \returns  a reference to the element
  inline T& operator() (unsigned int i, unsigned int j)
  { return m[i * 3 + j]; }

  /// \brief    operator giving element access
  /// \param    i row of the element to be accessed
  /// \param    j column of the element to be accessed
  /// \returns  a copy of the element
  inline T operator() (unsigned int i, unsigned int j) const
  { return m[i * 3 + j]; }

  //=================================================================================================================
  // modification functions
  //=================================================================================================================

  /// \brief    sets the matrix to an identity matrix
  inline void setIdentity ()
  {
    for (unsigned int i = 0; i < 3; ++i)
      for (unsigned int j = 0; j < 3; ++j)
        (*this)(i, j) = (i == j) ? (T)1 : (T)0;
  }

  //=================================================================================================================
  // other functions
  //=================================================================================================================

  /// \brief    calculates the inverse of the current matrix
  /// \returns  the inverse of the current matrix
  /// \note   appropriate invertibility is assumed
  inline TMatrix3x3 inverse () const
  {
    TMatrix3x3 output;

    output(0,0) = +((*this)(1, 1) * (*this)(2, 2) - (*this)(2, 1) * (*this)(1, 2));
    output(1,0) = -((*this)(0, 1) * (*this)(2, 2) - (*this)(0, 2) * (*this)(2, 1));
    output(2,0) = +((*this)(0, 1) * (*this)(1, 2) - (*this)(0, 2) * (*this)(1, 1));
    output(0,1) = -((*this)(1, 0) * (*this)(2, 2) - (*this)(1, 2) * (*this)(2, 0));
    output(1,1) = +((*this)(0, 0) * (*this)(2, 2) - (*this)(0, 2) * (*this)(2, 0));
    output(2,1) = -((*this)(0, 0) * (*this)(1, 2) - (*this)(1, 0) * (*this)(0, 2));
    output(0,2) = +((*this)(1, 0) * (*this)(2, 1) - (*this)(2, 0) * (*this)(1, 1));
    output(1,2) = -((*this)(0, 0) * (*this)(2, 1) - (*this)(2, 0) * (*this)(0, 1));
    output(2,2) = +((*this)(0, 0) * (*this)(1, 1) - (*this)(1, 0) * (*this)(0, 1));

    float det = +(*this)(0, 0) * ((*this)(1, 1) * (*this)(2, 2) - (*this)(2, 1) * (*this)(1, 2))
          -(*this)(0, 1) * ((*this)(1, 0) * (*this)(2, 2) - (*this)(1, 2) * (*this)(2, 0))
          +(*this)(0, 2) * ((*this)(1, 0) * (*this)(2, 1) - (*this)(1, 1) * (*this)(2, 0));

    det = 1.f / det;

    for (unsigned int i = 0; i < 9; i++)
      output.m[i] = output.m[i] * det;

    return output;
  }

public:
  T   m[9];             ///< the actual data
};

/// \brief    operator multiplying two matrices
/// \param    lhs   left hand side matrix
/// \param    rhs   right hand side matrix
/// \returns  the product of the two matrices
template <typename T>
inline TMatrix3x3<T> operator* (const TMatrix3x3<T>& lhs, const TMatrix3x3<T>& rhs)
{
  TMatrix3x3<T> output;

  output(0, 0) = lhs(0, 0) * rhs(0, 0) + lhs(0, 1) * rhs(1, 0) + lhs(0, 2) * rhs(2, 0);
  output(0, 1) = lhs(0, 0) * rhs(0, 1) + lhs(0, 1) * rhs(1, 1) + lhs(0, 2) * rhs(2, 1);
  output(0, 2) = lhs(0, 0) * rhs(0, 2) + lhs(0, 1) * rhs(1, 2) + lhs(0, 2) * rhs(2, 2);

  output(1, 0) = lhs(1, 0) * rhs(0, 0) + lhs(1, 1) * rhs(1, 0) + lhs(1, 2) * rhs(2, 0);
  output(1, 1) = lhs(1, 0) * rhs(0, 1) + lhs(1, 1) * rhs(1, 1) + lhs(1, 2) * rhs(2, 1);
  output(1, 2) = lhs(1, 0) * rhs(0, 2) + lhs(1, 1) * rhs(1, 2) + lhs(1, 2) * rhs(2, 2);

  output(2, 0) = lhs(2, 0) * rhs(0, 0) + lhs(2, 1) * rhs(1, 0) + lhs(2, 2) * rhs(2, 0);
  output(2, 1) = lhs(2, 0) * rhs(0, 1) + lhs(2, 1) * rhs(1, 1) + lhs(2, 2) * rhs(2, 1);
  output(2, 2) = lhs(2, 0) * rhs(0, 2) + lhs(2, 1) * rhs(1, 2) + lhs(2, 2) * rhs(2, 2);

  return output;
}

/// \brief    operator multiplying a vector with a matrix
/// \param    lhs   left hand side vector
/// \param    rhs   right hand side matrix
/// \returns  the product of the two matrices
template <typename T>
inline TVec3<T> operator* (const TVec3<T>& lhs, const TMatrix3x3<T>& rhs)
{
  TVec3<T> output;

  output[0] = lhs[0] * rhs(0, 0) + lhs[1] * rhs(1, 0) + lhs[2] * rhs(2, 0);
  output[1] = lhs[0] * rhs(0, 1) + lhs[1] * rhs(1, 1) + lhs[2] * rhs(2, 1);
  output[2] = lhs[0] * rhs(0, 2) + lhs[1] * rhs(1, 2) + lhs[2] * rhs(2, 2);

  return output;
}

/// \brief    operator printing the contents of a matrix to an output stream
/// \param    os    the output stream to be written to
/// \param    matrix  the matrix to be printed
/// \returns  the given output stream reference
template <typename T>
inline std::ostream& operator<< (std::ostream &os, const TMatrix3x3<T> &matrix)
{
  os << "[ "
    << matrix(0, 0) << ",\t"  << matrix(0, 1) << ",\t"  << matrix(0, 2) << ";\n"
    << matrix(1, 0) << ",\t"  << matrix(1, 1) << ",\t"  << matrix(1, 2) << ";\n"
    << matrix(2, 0) << ",\t"  << matrix(2, 1) << ",\t"  << matrix(2, 2) << " ]";

  return os;
}

/// \brief    generates a rotation matrix from an axis and an angle
/// \param    axis  the rotation axis
/// \param    angle the rotation angle
/// \returns  the rotation matrix
/// \note   implementation is done according to http://science.kennesaw.edu/~plaval/math4490/rotgen.pdf
template <typename T>
static inline TMatrix3x3<T> matrixRotationAxis (const TVec3<T>& axis, float angle)
{
  float c = cos(angle);
  float s = sin(angle);
  float t = 1.f - c;
  Vec3 a = normalize(axis);

  TMatrix3x3<T> output;

  output(0, 0) = t * a.x * a.x + c;
  output(0, 1) = t * a.x * a.y - s * a.z;
  output(0, 2) = t * a.x * a.z + s * a.y;

  output(1, 0) = t * a.x * a.y + s * a.z;
  output(1, 1) = t * a.y * a.y + c;
  output(1, 2) = t * a.y * a.z - s * a.x;

  output(2, 0) = t * a.x * a.z - s * a.y;
  output(2, 1) = t * a.y * a.z + s * a.x;
  output(2, 2) = t * a.z * a.z + c;

  return output;
}

//=====================================================================================================================
// class
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \class  TMatrix4x4
/// \brief  Represents a basic 4 by 4 square matrix
//
//=====================================================================================================================
template <typename T>
class TMatrix4x4
{
public:
  //=================================================================================================================
  // constructors / destructor
  //=================================================================================================================

  /// \brief    default constructor for the TMatrix4x4 class; initializes an identity matrix
  TMatrix4x4 ()
  {
    for (unsigned int i = 0; i < 4; ++i)
      for (unsigned int j = 0; j < 4; ++j)
        (*this)(i, j) = (i == j) ? (T)1 : (T)0;
  }

  /// \brief    copy constructor for the TMatrix4x4 class
  /// \param    other the source of the copy operation
  TMatrix4x4 (const TMatrix4x4<T>& other)
  { std::copy(other.m, other.m + 16, m); }

  /// \brief    constructor for the TMatrix4x4 class; initializes every element with the given value
  /// \param    value initial value for every matrix element
  explicit TMatrix4x4 (T value)
  {
    for (unsigned int i = 0; i < 4; ++i)
      for (unsigned int j = 0; j < 4; ++j)
        (*this)(i, j) = value;
  }

  /// \brief    constructor for the TMatrix4x4 class; copies the given data
  /// \param    mat   initial values for each matrix element
  explicit TMatrix4x4 (T mat[16])
  { std::copy(mat, mat + 16, m); }

  /// \brief    constructor for the TMatrix4x4 class; copies the given data
  /// \param    a00   element (0, 0) of the source matrix
  /// \param    a01   element (0, 1) of the source matrix
  /// \param    a02   element (0, 2) of the source matrix
  /// \param    a03   element (0, 3) of the source matrix
  /// \param    a10   element (1, 0) of the source matrix
  /// \param    a11   element (1, 1) of the source matrix
  /// \param    a12   element (1, 2) of the source matrix
  /// \param    a13   element (1, 3) of the source matrix
  /// \param    a20   element (2, 0) of the source matrix
  /// \param    a21   element (2, 1) of the source matrix
  /// \param    a22   element (2, 2) of the source matrix
  /// \param    a23   element (2, 3) of the source matrix
  /// \param    a30   element (3, 0) of the source matrix
  /// \param    a31   element (3, 1) of the source matrix
  /// \param    a32   element (3, 2) of the source matrix
  /// \param    a33   element (3, 3) of the source matrix
  explicit TMatrix4x4 (T a00, T a01, T a02, T a03, T a10, T a11, T a12, T a13,
             T a20, T a21, T a22, T a23, T a30, T a31, T a32, T a33)
  {
    (*this)(0, 0) = a00;
    (*this)(0, 1) = a01;
    (*this)(0, 2) = a02;
    (*this)(0, 3) = a03;
    (*this)(1, 0) = a10;
    (*this)(1, 1) = a11;
    (*this)(1, 2) = a12;
    (*this)(1, 3) = a13;
    (*this)(2, 0) = a20;
    (*this)(2, 1) = a21;
    (*this)(2, 2) = a22;
    (*this)(2, 3) = a23;
    (*this)(3, 0) = a30;
    (*this)(3, 1) = a31;
    (*this)(3, 2) = a32;
    (*this)(3, 3) = a33;
  }

  /// \brief    default destructor for the TMatrix4x4 class
  ~TMatrix4x4 () {}

  //=================================================================================================================
  // operators
  //=================================================================================================================

  /// \brief    operator giving element access
  /// \param    i row of the element to be accessed
  /// \param    j column of the element to be accessed
  /// \returns  a reference to the element
  inline T& operator() (unsigned int i, unsigned int j)
  { return m[i * 4 + j]; }

  /// \brief    operator giving element access
  /// \param    i row of the element to be accessed
  /// \param    j column of the element to be accessed
  /// \returns  a copy of the element
  inline T operator() (unsigned int i, unsigned int j) const
  { return m[i * 4 + j]; }

    /// \brief    operator giving element access
  /// \param    i index of the element to be accessed
  /// \returns  a reference to the element
  inline T& operator[] (unsigned int i)
  { return m[i]; }

  /// \brief    operator giving element access
  /// \param    i index of the element to be accessed
  /// \returns  a copy of the element
  inline T operator[] (unsigned int i) const
  { return m[i]; }


    //=================================================================================================================
  // modification functions
  //=================================================================================================================

  /// \brief    sets the matrix to an identity matrix
  inline void setIdentity ()
  {
    for (unsigned int i = 0; i < 4; ++i)
      for (unsigned int j = 0; j < 4; ++j)
        (*this)(i, j) = (i == j) ? (T)1 : (T)0;
  }

  //=================================================================================================================
  // other functions
  //=================================================================================================================

  /// \brief    calculates the inverse of the current matrix
  /// \returns  the inverse of the current matrix
  /// \note   appropriate invertibility is assumed
  inline TMatrix4x4 inverse () const
  {
    TMatrix4x4 output;

    output(0, 0) =  + (*this)(1, 1) * (*this)(2, 2) * (*this)(3, 3) + (*this)(2, 1) * (*this)(1, 3) * (*this)(3, 2) + (*this)(3, 1) * (*this)(1, 2) * (*this)(2, 3)
            - (*this)(1, 1) * (*this)(2, 3) * (*this)(3, 2) - (*this)(2, 1) * (*this)(1, 2) * (*this)(3, 3) - (*this)(3, 1) * (*this)(1, 3) * (*this)(2, 2);
    output(1, 0) =  + (*this)(1, 0) * (*this)(2, 3) * (*this)(3, 2) + (*this)(2, 0) * (*this)(1, 2) * (*this)(3, 3) + (*this)(3, 0) * (*this)(1, 3) * (*this)(2, 2)
            - (*this)(1, 0) * (*this)(2, 2) * (*this)(3, 3) - (*this)(2, 0) * (*this)(1, 3) * (*this)(3, 2) - (*this)(3, 0) * (*this)(1, 2) * (*this)(2, 3);
    output(2, 0) =  + (*this)(1, 0) * (*this)(2, 1) * (*this)(3, 3) + (*this)(2, 0) * (*this)(1, 3) * (*this)(3, 1) + (*this)(3, 0) * (*this)(1, 1) * (*this)(2, 3)
            - (*this)(1, 0) * (*this)(2, 3) * (*this)(3, 1) - (*this)(2, 0) * (*this)(1, 1) * (*this)(3, 3) - (*this)(3, 0) * (*this)(1, 3) * (*this)(2, 1);
    output(3, 0) =  + (*this)(1, 0) * (*this)(2, 2) * (*this)(3, 1) + (*this)(2, 0) * (*this)(1, 1) * (*this)(3, 2) + (*this)(3, 0) * (*this)(1, 2) * (*this)(2, 1)
            - (*this)(1, 0) * (*this)(2, 1) * (*this)(3, 2) - (*this)(2, 0) * (*this)(1, 2) * (*this)(3, 1) - (*this)(3, 0) * (*this)(1, 1) * (*this)(2, 2);

    output(0, 1) =  + (*this)(0, 1) * (*this)(2, 3) * (*this)(3, 2) + (*this)(2, 1) * (*this)(0, 2) * (*this)(3, 3) + (*this)(3, 1) * (*this)(0, 3) * (*this)(2, 2)
            - (*this)(0, 1) * (*this)(2, 2) * (*this)(3, 3) - (*this)(2, 1) * (*this)(0, 3) * (*this)(3, 2) - (*this)(3, 1) * (*this)(0, 2) * (*this)(2, 3);
    output(1, 1) =  + (*this)(0, 0) * (*this)(2, 2) * (*this)(3, 3) + (*this)(2, 0) * (*this)(0, 3) * (*this)(3, 2) + (*this)(3, 0) * (*this)(0, 2) * (*this)(2, 3)
            - (*this)(0, 0) * (*this)(2, 3) * (*this)(3, 2) - (*this)(2, 0) * (*this)(0, 2) * (*this)(3, 3) - (*this)(3, 0) * (*this)(0, 3) * (*this)(2, 2);
    output(2, 1) =  + (*this)(0, 0) * (*this)(2, 3) * (*this)(3, 1) + (*this)(2, 0) * (*this)(0, 1) * (*this)(3, 3) + (*this)(3, 0) * (*this)(0, 3) * (*this)(2, 1)
            - (*this)(0, 0) * (*this)(2, 1) * (*this)(3, 3) - (*this)(2, 0) * (*this)(0, 3) * (*this)(3, 1) - (*this)(3, 0) * (*this)(0, 1) * (*this)(2, 3);
    output(3, 1) =  + (*this)(0, 0) * (*this)(2, 1) * (*this)(3, 2) + (*this)(2, 0) * (*this)(0, 2) * (*this)(3, 1) + (*this)(3, 0) * (*this)(0, 1) * (*this)(2, 2)
            - (*this)(0, 0) * (*this)(2, 2) * (*this)(3, 1) - (*this)(2, 0) * (*this)(0, 1) * (*this)(3, 2) - (*this)(3, 0) * (*this)(0, 2) * (*this)(2, 1);

    output(0, 2) =  + (*this)(0, 1) * (*this)(1, 2) * (*this)(3, 3) + (*this)(1, 1) * (*this)(0, 3) * (*this)(3, 2) + (*this)(3, 1) * (*this)(0, 2) * (*this)(1, 3)
            - (*this)(0, 1) * (*this)(1, 3) * (*this)(3, 2) - (*this)(1, 1) * (*this)(0, 2) * (*this)(3, 3) - (*this)(3, 1) * (*this)(0, 3) * (*this)(1, 2);
    output(1, 2) =  + (*this)(0, 0) * (*this)(1, 3) * (*this)(3, 2) + (*this)(1, 0) * (*this)(0, 2) * (*this)(3, 3) + (*this)(3, 0) * (*this)(0, 3) * (*this)(1, 2)
            - (*this)(0, 0) * (*this)(1, 2) * (*this)(3, 3) - (*this)(1, 0) * (*this)(0, 3) * (*this)(3, 2) - (*this)(3, 0) * (*this)(0, 2) * (*this)(1, 3);
    output(2, 2) =  + (*this)(0, 0) * (*this)(1, 1) * (*this)(3, 3) + (*this)(1, 0) * (*this)(0, 3) * (*this)(3, 1) + (*this)(3, 0) * (*this)(0, 1) * (*this)(1, 3)
            - (*this)(0, 0) * (*this)(1, 3) * (*this)(3, 1) - (*this)(1, 0) * (*this)(0, 1) * (*this)(3, 3) - (*this)(3, 0) * (*this)(0, 3) * (*this)(1, 1);
    output(3, 2) =  + (*this)(0, 0) * (*this)(1, 2) * (*this)(3, 1) + (*this)(1, 0) * (*this)(0, 1) * (*this)(3, 2) + (*this)(3, 0) * (*this)(0, 2) * (*this)(1, 1)
            - (*this)(0, 0) * (*this)(1, 1) * (*this)(3, 2) - (*this)(1, 0) * (*this)(0, 2) * (*this)(3, 1) - (*this)(3, 0) * (*this)(0, 1) * (*this)(1, 2);

    output(0, 3) =  + (*this)(0, 1) * (*this)(1, 3) * (*this)(2, 2) + (*this)(1, 1) * (*this)(0, 2) * (*this)(2, 3) + (*this)(2, 1) * (*this)(0, 3) * (*this)(1, 2)
            - (*this)(0, 1) * (*this)(1, 2) * (*this)(2, 3) - (*this)(1, 1) * (*this)(0, 3) * (*this)(2, 2) - (*this)(2, 1) * (*this)(0, 2) * (*this)(1, 3);
    output(1, 3) =  + (*this)(0, 0) * (*this)(1, 2) * (*this)(2, 3) + (*this)(1, 0) * (*this)(0, 3) * (*this)(2, 2) + (*this)(2, 0) * (*this)(0, 2) * (*this)(1, 3)
            - (*this)(0, 0) * (*this)(1, 3) * (*this)(2, 2) - (*this)(1, 0) * (*this)(0, 2) * (*this)(2, 3) - (*this)(2, 0) * (*this)(0, 3) * (*this)(1, 2);
    output(2, 3) =  + (*this)(0, 0) * (*this)(1, 3) * (*this)(2, 1) + (*this)(1, 0) * (*this)(0, 1) * (*this)(2, 3) + (*this)(2, 0) * (*this)(0, 3) * (*this)(1, 1)
            - (*this)(0, 0) * (*this)(1, 1) * (*this)(2, 3) - (*this)(1, 0) * (*this)(0, 3) * (*this)(2, 1) - (*this)(2, 0) * (*this)(0, 1) * (*this)(1, 3);
    output(3, 3) =  + (*this)(0, 0) * (*this)(1, 1) * (*this)(2, 2) + (*this)(1, 0) * (*this)(0, 2) * (*this)(2, 1) + (*this)(2, 0) * (*this)(0, 1) * (*this)(1, 2)
            - (*this)(0, 0) * (*this)(1, 2) * (*this)(2, 1) - (*this)(1, 0) * (*this)(0, 1) * (*this)(2, 2) - (*this)(2, 0) * (*this)(0, 2) * (*this)(1, 1);

    float det = (*this)(0, 0) * output(0, 0) + (*this)(0, 1) * output(1, 0) + (*this)(0, 2) * output(2, 0) + (*this)(0, 3) * output(3, 0);
    det = 1.f / det;

    for (unsigned int i = 0; i < 16; i++)
      output.m[i] = output.m[i] * det;

    return output;
  }

public:
  T   m[16];              ///< the actual data
};

/// \brief    operator multiplying two matrices
/// \param    lhs   left hand side matrix
/// \param    rhs   right hand side matrix
/// \returns  the product of the two matrices
template <typename T>
inline TMatrix4x4<T> operator* (const TMatrix4x4<T>& lhs, const TMatrix4x4<T>& rhs)
{
  TMatrix4x4<T> output;

  output(0, 0) = lhs(0, 0) * rhs(0, 0) + lhs(0, 1) * rhs(1, 0) + lhs(0, 2) * rhs(2, 0) + lhs(0, 3) * rhs(3, 0);
  output(0, 1) = lhs(0, 0) * rhs(0, 1) + lhs(0, 1) * rhs(1, 1) + lhs(0, 2) * rhs(2, 1) + lhs(0, 3) * rhs(3, 1);
  output(0, 2) = lhs(0, 0) * rhs(0, 2) + lhs(0, 1) * rhs(1, 2) + lhs(0, 2) * rhs(2, 2) + lhs(0, 3) * rhs(3, 2);
  output(0, 3) = lhs(0, 0) * rhs(0, 3) + lhs(0, 1) * rhs(1, 3) + lhs(0, 2) * rhs(2, 3) + lhs(0, 3) * rhs(3, 3);

  output(1, 0) = lhs(1, 0) * rhs(0, 0) + lhs(1, 1) * rhs(1, 0) + lhs(1, 2) * rhs(2, 0) + lhs(1, 3) * rhs(3, 0);
  output(1, 1) = lhs(1, 0) * rhs(0, 1) + lhs(1, 1) * rhs(1, 1) + lhs(1, 2) * rhs(2, 1) + lhs(1, 3) * rhs(3, 1);
  output(1, 2) = lhs(1, 0) * rhs(0, 2) + lhs(1, 1) * rhs(1, 2) + lhs(1, 2) * rhs(2, 2) + lhs(1, 3) * rhs(3, 2);
  output(1, 3) = lhs(1, 0) * rhs(0, 3) + lhs(1, 1) * rhs(1, 3) + lhs(1, 2) * rhs(2, 3) + lhs(1, 3) * rhs(3, 3);

  output(2, 0) = lhs(2, 0) * rhs(0, 0) + lhs(2, 1) * rhs(1, 0) + lhs(2, 2) * rhs(2, 0) + lhs(2, 3) * rhs(3, 0);
  output(2, 1) = lhs(2, 0) * rhs(0, 1) + lhs(2, 1) * rhs(1, 1) + lhs(2, 2) * rhs(2, 1) + lhs(2, 3) * rhs(3, 1);
  output(2, 2) = lhs(2, 0) * rhs(0, 2) + lhs(2, 1) * rhs(1, 2) + lhs(2, 2) * rhs(2, 2) + lhs(2, 3) * rhs(3, 2);
  output(2, 3) = lhs(2, 0) * rhs(0, 3) + lhs(2, 1) * rhs(1, 3) + lhs(2, 2) * rhs(2, 3) + lhs(2, 3) * rhs(3, 3);

  output(3, 0) = lhs(3, 0) * rhs(0, 0) + lhs(3, 1) * rhs(1, 0) + lhs(3, 2) * rhs(2, 0) + lhs(3, 3) * rhs(3, 0);
  output(3, 1) = lhs(3, 0) * rhs(0, 1) + lhs(3, 1) * rhs(1, 1) + lhs(3, 2) * rhs(2, 1) + lhs(3, 3) * rhs(3, 1);
  output(3, 2) = lhs(3, 0) * rhs(0, 2) + lhs(3, 1) * rhs(1, 2) + lhs(3, 2) * rhs(2, 2) + lhs(3, 3) * rhs(3, 2);
  output(3, 3) = lhs(3, 0) * rhs(0, 3) + lhs(3, 1) * rhs(1, 3) + lhs(3, 2) * rhs(2, 3) + lhs(3, 3) * rhs(3, 3);

  return output;
}

/// \brief    operator multiplying a vector with a matrix
/// \param    lhs   left hand side vector
/// \param    rhs   right hand side matrix
/// \returns  the product of the two matrices
template <typename T>
inline TVec4<T> operator* (const TVec4<T>& lhs, const TMatrix4x4<T>& rhs)
{
  TVec4<T> output;

  output[0] = lhs[0] * rhs(0, 0) + lhs[1] * rhs(1, 0) + lhs[2] * rhs(2, 0) + lhs[3] * rhs(3, 0);
  output[1] = lhs[0] * rhs(0, 1) + lhs[1] * rhs(1, 1) + lhs[2] * rhs(2, 1) + lhs[3] * rhs(3, 1);
  output[2] = lhs[0] * rhs(0, 2) + lhs[1] * rhs(1, 2) + lhs[2] * rhs(2, 2) + lhs[3] * rhs(3, 2);
  output[3] = lhs[0] * rhs(0, 3) + lhs[1] * rhs(1, 3) + lhs[2] * rhs(2, 3) + lhs[3] * rhs(3, 3);

  return output;
}

/// \brief    operator printing the contents of a matrix to an output stream
/// \param    os    the output stream to be written to
/// \param    matrix  the matrix to be printed
/// \returns  the given output stream reference
template <typename T>
inline std::ostream& operator<< (std::ostream &os, const TMatrix4x4<T> &matrix)
{
  os << "[ "
    << matrix(0, 0) << ",\t"  << matrix(0, 1) << ",\t"  << matrix(0, 2) << ",\t"  << matrix(0, 3) << ";\n"
    << matrix(1, 0) << ",\t"  << matrix(1, 1) << ",\t"  << matrix(1, 2) << ",\t"  << matrix(1, 3) << ";\n"
    << matrix(2, 0) << ",\t"  << matrix(2, 1) << ",\t"  << matrix(2, 2) << ",\t"  << matrix(2, 3) << ";\n"
    << matrix(3, 0) << ",\t"  << matrix(3, 1) << ",\t"  << matrix(3, 2) << ",\t"  << matrix(3, 3) << " ]";

  return os;
}
"""
  }
}
