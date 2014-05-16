package exastencils.util

import java.io.PrintWriter
import java.io.File
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._

case class Vector() extends Node with FilePrettyPrintable {
  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"Util/Vector.h");

    writer << ("""
//=====================================================================================================================
//                                        _    __   ____   ____     ______   ____
//                                       | |  / /  /  _/  / __ \   / ____/  / __ \
//                                       | | / /   / /   / /_/ /  / __/    / /_/ /
//                                       | |/ /  _/ /   / ____/  / /___   / _, _/
//                                       |___/  /___/  /_/      /_____/  /_/ |_|
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	Vector.h
/// \brief	Header file for the Vector classes
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// includes
//=====================================================================================================================

#define _USE_MATH_DEFINES
#include <iostream>
#include <sstream>
#include <string>
#include <cmath>

//=====================================================================================================================
// forward declarations
//=====================================================================================================================

template <typename T> class TVec4;
template <typename T> class TVec3;
template <typename T> class TVec2;

//=====================================================================================================================
// typedefs
//=====================================================================================================================

typedef TVec4<double> Vec4;				///< a 4D float vector
typedef TVec3<double> Vec3;				///< a 3D float vector
typedef TVec2<double> Vec2;				///< a 2D float vector

typedef TVec4<unsigned int> Vec4u;			///< a 4D unsigned int vector
typedef TVec3<unsigned int> Vec3u;			///< a 3D unsigned int vector
typedef TVec2<unsigned int> Vec2u;			///< a 2D unsigned int vector

//=====================================================================================================================
// class
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \class	TVec2
/// \brief	Represents a basic vector with 2 elements
//
//=====================================================================================================================
template <typename T>
class TVec2
{
public:
	//=================================================================================================================
	// constructors / destructor
	//=================================================================================================================

	/// \brief		default constructor for the TVec2 class; initializes each element with 0
	TVec2 ()
		: x((T)0), y((T)0)
	{}

	/// \brief		copy constructor for the TVec2 class
	/// \param		other	the source of the copy operation
	TVec2 (const TVec2& other)
		: x(other.x), y(other.y)
	{}

	/// \brief		constructor for the TVec2 class; initializes every element with the given value
	/// \param		val		initial value for every vector element
	explicit TVec2 (T val)
		: x(val), y(val)
	{}

	/// \brief		constructor for the TVec2 class; copies the given data
	/// \param		x		the x element of the source vector
	/// \param		y		the y element of the source vector
	explicit TVec2 (T x, T y)
		: x(x), y(y)
	{}

	/// \brief		constructor for the TVec2 class; copies the given data
	/// \param		other	initial values for each vector element
	explicit TVec2 (const T other[2])
		: x(other[0]), y(other[1])
	{}

	// may, for some reason, not have a default destructor

	//=================================================================================================================
	// operators
	//=================================================================================================================

	/// \brief		operator giving element access
	/// \param		i		index of the element to be accessed
	/// \returns	a reference to the element
	inline T& operator[] (int i)
	{ return v[i]; }

	/// \brief		operator giving element access
	/// \param		i		index of the element to be accessed
	/// \returns	a copy of the element
	inline T operator[] (int i) const
	{ return v[i]; }

	/// \brief		operator for the unary minus
	/// \returns	a vector with negated values
	inline TVec2 operator- () const
	{ return TVec2(-x, -y); }

	/// \brief		operator adding two vectors
	/// \param		other	the vector to be added to the current one
	/// \returns	a new vector with the sum of the given ones
	inline TVec2 operator+ (const TVec2 &other) const
	{ return TVec2(x + other.x, y + other.y); }

	/// \brief		operator subtracting two vectors
	/// \param		other	the vector to be subtracted from the current one
	/// \returns	a vector with the result of the subtraction
	inline TVec2 operator- (const TVec2 &other) const
	{ return TVec2(x - other.x, y - other.y); }

	/// \brief		operator representing a scalar multiplication
	/// \param		s		the scalar value to be multiplied with
	/// \returns	a vector with the result of the scalar multiplication
	inline TVec2 operator* (const T s) const
	{ return TVec2(x * s, y * s); }

	/// \brief		operator representing a scalar division
	/// \param		s		the scalar value to be divided with
	/// \returns	a vector with the result of the scalar division
	inline TVec2 operator/ (const T s) const
	{ return TVec2( x / s, y / s); }

	/// \brief		operator adding a vector to the current one
	/// \param		other	the vector to be added
	/// \returns	a reference to the current vector
	inline TVec2& operator+= (const TVec2 &other)
	{ x += other.x; y += other.y; return *this; }

	/// \brief		operator subtracting a vector from the current one
	/// \param		other	the vector to be subtracted
	/// \returns	a reference to the current vector
	inline TVec2& operator-= (const TVec2 &other)
	{ x -= other.x; y -= other.y; return *this; }

	/// \brief		operator performing a scalar multiplication
	/// \param		s		the scalar value to be multiplied with
	/// \returns	a reference to the current vector
	inline TVec2& operator*= (const T s)
	{ x *= s; y *= s; return *this; }

	/// \brief		operator performing a scalar division
	/// \param		s		the scalar value to be divided with
	/// \returns	a reference to the current vector
	inline TVec2& operator/= (const T s)
	{ x /= s; y /= s; return *this; }

	/// \brief		operator checking for element wise equality of two vectors
	/// \param		other	the vector to be checked against
	/// \returns	true if the given vector is (element wise) equal to the current one, false otherwise
	inline bool operator== (const TVec2 &other) const
	{ return (x == other.x && y == other.y); }

	/// \brief		operator checking for element wise inequality of two vectors
	/// \param		other	the vector to be checked against
	/// \returns	false if the given vector is (element wise) equal to the current one, true otherwise
	inline bool operator!= (const TVec2 &other) const
	{ return (x != other.x || y != other.y); }

	//=================================================================================================================
	// arithmetic functions
	//=================================================================================================================

	/// \brief		calculates the length of the current vector
	/// \returns	the length of the current vector
	inline T length () const
	{ return std::sqrt(lengthSquared()); }

	/// \brief		calculates the squared length of the current vector
	/// \returns	the squared length of the current vector
	inline T lengthSquared () const
	{ return x * x + y * y; }

	/// \brief		calculates the sum of all elements of the current vector
	/// \returns	the sum of components
	inline T componentSum () const
	{ return x + y; }

	/// \brief		calculates the product of all elements of the current vector
	/// \returns	the product of components
	inline T componentProd () const
	{ return x * y; }

	//=================================================================================================================
	// other functions
	//=================================================================================================================

	/// \brief		checks if all elements of the current vector are equal to zero
	/// \returns	true if all elements are equal to zero, false otherwise
	inline bool isZero () const
	{ return (T)0 == x && (T)0 == y; }

	/// \brief		determines the minimum element of the current vector
	/// \returns	the minimum component
	inline T minComponent () const
	{ return std::min(x, y); }

	/// \brief		determines the maximum element of the current vector
	/// \returns	the maximum component
	inline T maxComponent () const
	{ return std::max(x, y); }

	/// \brief		generates a string representation of the current vector
	/// \returns	a string representing the current vector
	inline std::string toString () const
	{ std::ostringstream oss; oss << "[" << x << ", " << y << "]"; return oss.str(); }

public:
	union
	{
		T		v[2];			///< actual data of the vector
		struct	{ T x, y; };	///< alternate representation of the vector contents
	};
};

/// \brief		operator multiplying two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the product of the two vectors
template <typename T>
inline TVec2<T> operator* (const TVec2<T> &v1, const TVec2<T> &v2)
{ return TVec2<T>(v1.x * v2.x, v1.y * v2.y); }

/// \brief		operator multiplying a scalar and a vector
/// \param		s		left hand side scalar
/// \param		vec		right hand side vector
/// \returns	the product of the scalar and the vector
template <typename T>
inline TVec2<T> operator* (T s, const TVec2<T> &vec)
{ return vec * s; }

/// \brief		operator dividing one vector with another (element wise)
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	a new vector initialized with the element wise division results
template <typename T>
inline TVec2<T> operator/ (const TVec2<T> &v1, const TVec2<T> &v2)
{ return TVec2<T>(v1.x / v2.x, v1.y / v2.y); }

/// \brief		operator printing the contents of a vector to an output stream
/// \param		os		the output stream to be written to
/// \param		vec		the vector to be printed
/// \returns	the given output stream reference
template <typename T>
inline std::ostream& operator<< (std::ostream &os, const TVec2<T> &vec)
{ os << "[ " << vec.x << ", " << vec.y << " ]"; return os; }

//=================================================================================================================
// utility functions
//=================================================================================================================

/// \brief		normalizes the given vector
/// \param		vec		the vector to be normalized
/// \returns	a normalized copy of the given vector
/// \note		in case of a vector length of 0, a copy of the original vector is returned
template <typename T>
inline TVec2<T> normalize (const TVec2<T> &vec)
{ T l = vec.length(); return (((T)0 == l) ? vec : vec / l); }

/// \brief		calculates the dot product of two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the dot product of the given vectors
template <typename T>
inline T dot (const TVec2<T> &v1, const TVec2<T> &v2)
{ return v1.x * v2.x + v1.y * v2.y; }

/// \brief		calculates the absolute value of the dot product of two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the absolute value of the dot product of the given vectors
template <typename T>
inline T absDot (const TVec2<T> &v1, const TVec2<T> &v2)
{ return std::abs(dot(v1, v2)); }

/// \brief		determines the element wise minimum between two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	a vector initialized with the element wise minimum values of the given vectors
template <typename T>
inline TVec2<T> min (const TVec2<T> &v1, const TVec2<T> &v2)
{ return TVec2<T>(std::min(v1.x, v2.x), std::min(v1.y, v2.y)); }

/// \brief		determines the element wise maximum between two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	a vector initialized with the element wise maximum values of the given vectors
template <typename T>
inline TVec2<T> max (const TVec2<T> &v1, const TVec2<T> &v2)
{ return TVec2<T>(std::max(v1.x, v2.x), std::max(v1.y, v2.y)); }

//=====================================================================================================================
// class
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \class	TVec3
/// \brief	Represents a basic vector with 3 elements
//
//=====================================================================================================================
template <typename T>
class TVec3
{
public:
	//=================================================================================================================
	// constructors / destructor
	//=================================================================================================================

	/// \brief		default constructor for the TVec3 class; initializes each element with 0
	TVec3 ()
		: x((T)0), y((T)0), z((T)0)
	{}

	/// \brief		copy constructor for the TVec3 class
	/// \param		other	the source of the copy operation
	TVec3 (const TVec3& other)
		: x(other.x), y(other.y), z(other.z)
	{}

	/// \brief		constructor for the TVec3 class; initializes with the values from the given vector and remaining values with 0
	/// \param		vec		the input vector
	explicit TVec3 (const TVec2<T>& vec)
		: x(vec.x), y(vec.y), z(0)
	{}

	/// \brief		constructor for the TVec3 class; initializes every element with the given value
	/// \param		val		initial value for every vector element
	explicit TVec3 (T val)
		: x(val), y(val), z(val)
	{}

	/// \brief		constructor for the TVec3 class; copies the given data
	/// \param		x		the x element of the source vector
	/// \param		y		the y element of the source vector
	/// \param		z		the z element of the source vector
	explicit TVec3 (T x, T y, T z)
		: x(x), y(y), z(z)
	{}

	/// \brief		constructor for the TVec3 class; initializes using the contents of the given vector and the scalar
	/// \param		vec		the input vector
	/// \param		z_		the w element of the vector
	explicit TVec3 (const TVec2<T>& vec, T z_)
		: x(vec.x), y(vec.y), z(z_)
	{}

	/// \brief		constructor for the TVec3 class; copies the given data
	/// \param		other	initial values for each vector element
	explicit TVec3 (const T other[3])
		: x(other[0]), y(other[1]), z(other[2])
	{}

	// may, for some reason, not have a default destructor

	//=================================================================================================================
	// operators
	//=================================================================================================================

	/// \brief		operator giving element access
	/// \param		i		index of the element to be accessed
	/// \returns	a reference to the element
	inline T& operator[] (int i)
	{ return v[i]; }

	/// \brief		operator giving element access
	/// \param		i		index of the element to be accessed
	/// \returns	a copy of the element
	inline T operator[] (int i) const
	{ return v[i]; }

	/// \brief		operator for the unary minus
	/// \returns	a vector with negated values
	inline TVec3 operator- () const
	{ return TVec3(-x, -y, -z); }

	/// \brief		operator adding two vectors
	/// \param		other	the vector to be added to the current one
	/// \returns	a new vector with the sum of the given ones
	inline TVec3 operator+ (const TVec3 &other) const
	{ return TVec3(x + other.x, y + other.y, z + other.z); }

	/// \brief		operator subtracting two vectors
	/// \param		other	the vector to be subtracted from the current one
	/// \returns	a vector with the result of the subtraction
	inline TVec3 operator- (const TVec3 &other) const
	{ return TVec3(x - other.x, y - other.y, z - other.z); }

	/// \brief		operator representing a scalar multiplication
	/// \param		s		the scalar value to be multiplied with
	/// \returns	a vector with the result of the scalar multiplication
	inline TVec3 operator* (const T s) const
	{ return TVec3(x * s, y * s, z * s); }

	/// \brief		operator representing a scalar division
	/// \param		s		the scalar value to be divided with
	/// \returns	a vector with the result of the scalar division
	inline TVec3 operator/ (const T s) const
	{ return TVec3( x / s, y / s, z / s); }

	/// \brief		operator adding a vector to the current one
	/// \param		other	the vector to be added
	/// \returns	a reference to the current vector
	inline TVec3& operator+= (const TVec3 &other)
	{ x += other.x; y += other.y; z += other.z; return *this; }

	/// \brief		operator subtracting a vector from the current one
	/// \param		other	the vector to be subtracted
	/// \returns	a reference to the current vector
	inline TVec3& operator-= (const TVec3 &other)
	{ x -= other.x; y -= other.y; z -= other.z; return *this; }

	/// \brief		operator performing a scalar multiplication
	/// \param		s		the scalar value to be multiplied with
	/// \returns	a reference to the current vector
	inline TVec3& operator*= (const T s)
	{ x *= s; y *= s; z *= s; return *this; }

	/// \brief		operator performing a scalar division
	/// \param		s		the scalar value to be divided with
	/// \returns	a reference to the current vector
	inline TVec3& operator/= (const T s)
	{ x /= s; y /= s; z /= s; return *this; }

	/// \brief		operator checking for element wise equality of two vectors
	/// \param		other	the vector to be checked against
	/// \returns	true if the given vector is (element wise) equal to the current one, false otherwise
	inline bool operator== (const TVec3 &other) const
	{ return (x == other.x && y == other.y && z == other.z); }

	/// \brief		operator checking for element wise inequality of two vectors
	/// \param		other	the vector to be checked against
	/// \returns	false if the given vector is (element wise) equal to the current one, true otherwise
	inline bool operator!= (const TVec3 &other) const
	{ return (x != other.x || y != other.y || z != other.z); }

	//=================================================================================================================
	// get functions
	//=================================================================================================================

	/// \brief		get function for the first 2 elements of the vector
	/// \returns	a 2D vector with the first 2 elements of the current vector
	inline TVec2<T> xy () const
	{ return TVec2<T>(x, y); }

	//=================================================================================================================
	// arithmetic functions
	//=================================================================================================================

	/// \brief		calculates the length of the current vector
	/// \returns	the length of the current vector
	inline T length () const
	{ return std::sqrt(lengthSquared()); }

	/// \brief		calculates the squared length of the current vector
	/// \returns	the squared length of the current vector
	inline T lengthSquared () const
	{ return x * x + y * y + z * z; }

	/// \brief		calculates the sum of all elements of the current vector
	/// \returns	the sum of components
	inline T componentSum () const
	{ return x + y + z; }

	/// \brief		calculates the product of all elements of the current vector
	/// \returns	the product of components
	inline T componentProd () const
	{ return x * y * z; }

	//=================================================================================================================
	// other functions
	//=================================================================================================================

	/// \brief		checks if all elements of the current vector are equal to zero
	/// \returns	true if all elements are equal to zero, false otherwise
	inline bool isZero () const
	{ return (T)0 == x && (T)0 == y && (T)0 == z; }

	/// \brief		determines the minimum element of the current vector
	/// \returns	the minimum component
	inline T minComponent () const
	{ return std::min(std::min(x, y), z); }

	/// \brief		determines the maximum element of the current vector
	/// \returns	the maximum component
	inline T maxComponent () const
	{ return std::max(std::max(x, y), z); }

	/// \brief		generates a string representation of the current vector
	/// \returns	a string representing the current vector
	inline std::string toString () const
	{ std::ostringstream oss; oss << "[" << x << ", " << y << ", " << z << "]"; return oss.str(); }

public:
	union
	{
		T		v[3];				///< actual data of the vector
		struct	{ T x, y, z; };		///< alternate representation of the vector contents
	};
};

/// \brief		operator multiplying two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the product of the two vectors
template <typename T>
inline TVec3<T> operator* (const TVec3<T> &v1, const TVec3<T> &v2)
{ return TVec3<T>(v1.x * v2.x, v1.y * v2.y, v1.z * v2.z); }

/// \brief		operator multiplying a scalar and a vector
/// \param		s		left hand side scalar
/// \param		vec		right hand side vector
/// \returns	the product of the scalar and the vector
template <typename T>
inline TVec3<T> operator* (T s, const TVec3<T> &vec)
{ return vec * s; }

/// \brief		operator dividing one vector with another (element wise)
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	a new vector initialized with the element wise division results
template <typename T>
inline TVec3<T> operator/ (const TVec3<T> &v1, const TVec3<T> &v2)
{ return TVec3<T>(v1.x / v2.x, v1.y / v2.y, v1.z / v2.z); }

/// \brief		operator printing the contents of a vector to an output stream
/// \param		os		the output stream to be written to
/// \param		vec		the vector to be printed
/// \returns	the given output stream reference
template <typename T>
inline std::ostream& operator<< (std::ostream &os, const TVec3<T> &vec)
{ os << "[ " << vec.x << ", " << vec.y << ", " << vec.z << " ]"; return os; }

//=================================================================================================================
// utility functions
//=================================================================================================================

/// \brief		normalizes the given vector
/// \param		vec		the vector to be normalized
/// \returns	a normalized copy of the given vector
/// \note		in case of a vector length of 0, a copy of the original vector is returned
template <typename T>
inline TVec3<T> normalize (const TVec3<T> &vec)
{ T l = vec.length(); return (((T)0 == l) ? vec : vec / l); }

/// \brief		calculates the dot product of two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the dot product of the given vectors
template <typename T>
inline T dot (const TVec3<T> &v1, const TVec3<T> &v2)
{ return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z; }

/// \brief		calculates the absolute value of the dot product of two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the absolute value of the dot product of the given vectors
template <typename T>
inline T absDot (const TVec3<T> &v1, const TVec3<T> &v2)
{ return std::abs(dot(v1, v2)); }

/// \brief		calculates the cross product of two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the cross product of the given vectors
template <typename T>
inline TVec3<T> cross (const TVec3<T> &v1, const TVec3<T> &v2)
{ return TVec3<T>(v1.y * v2.z - v1.z * v2.y, v1.z * v2.x - v1.x * v2.z, v1.x * v2.y - v1.y * v2.x); }

/// \brief		determines the element wise minimum between two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	a vector initialized with the element wise minimum values of the given vectors
template <typename T>
inline TVec3<T> min (const TVec3<T> &v1, const TVec3<T> &v2)
{ return TVec3<T>(std::min(v1.x, v2.x), std::min(v1.y, v2.y), std::min(v1.z, v2.z)); }

/// \brief		determines the element wise maximum between two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	a vector initialized with the element wise maximum values of the given vectors
template <typename T>
inline TVec3<T> max (const TVec3<T> &v1, const TVec3<T> &v2)
{ return TVec3<T>(std::max(v1.x, v2.x), std::max(v1.y, v2.y), std::max(v1.z, v2.z)); }

//=====================================================================================================================
// class
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \class	TVec4
/// \brief	Represents a basic vector with 4 elements
//
//=====================================================================================================================
template <typename T>
class TVec4
{
public:
	//=================================================================================================================
	// constructors / destructor
	//=================================================================================================================

	/// \brief		default constructor for the TVec4 class; initializes each element with 0
	TVec4 ()
		: x((T)0), y((T)0), z((T)0), w((T)0)
	{}

	/// \brief		copy constructor for the TVec4 class
	/// \param		other	the source of the copy operation
	TVec4 (const TVec4& other)
		: x(other.x), y(other.y), z(other.z), w(other.w)
	{}

	/// \brief		constructor for the TVec4 class; initializes using the contents of the given vector and the scalar
	/// \param		vec		the input vector
	/// \param		w_		the w element of the vector
	explicit TVec4 (const TVec3<T>& vec, T w_)
		: x(vec.x), y(vec.y), z(vec.z), w(w_)
	{}

	/// \brief		constructor for the TVec4 class; initializes with the values from the given vector and remaining values with 0
	/// \param		vec		the input vector
	explicit TVec4 (const TVec2<T>& vec)
		: x(vec.x), y(vec.y), z(0), w(0)
	{}

	/// \brief		constructor for the TVec4 class; initializes every element with the given value
	/// \param		val		initial value for every vector element
	explicit TVec4 (T val)
		: x(val), y(val), z(val), w(val)
	{}

	/// \brief		constructor for the TVec4 class; copies the given data
	/// \param		x		the x element of the source vector
	/// \param		y		the y element of the source vector
	/// \param		z		the z element of the source vector
	/// \param		w		the w element of the source vector
	explicit TVec4 (T x, T y, T z, T w)
		: x(x), y(y), z(z), w(w)
	{}

	/// \brief		constructor for the TVec4 class; copies the given data
	/// \param		other	initial values for each vector element
	explicit TVec4 (const T other[4])
		: x(other[0]), y(other[1]), z(other[2]), w(other[3])
	{}

	// may, for some reason, not have a default destructor

	//=================================================================================================================
	// operators
	//=================================================================================================================

	/// \brief		operator giving element access
	/// \param		i		index of the element to be accessed
	/// \returns	a reference to the element
	inline T& operator[] (int i)
	{ return v[i]; }

	/// \brief		operator giving element access
	/// \param		i		index of the element to be accessed
	/// \returns	a copy of the element
	inline T operator[] (int i) const
	{ return v[i]; }

	/// \brief		operator for the unary minus
	/// \returns	a vector with negated values
	inline TVec4 operator- () const
	{ return TVec4(-x, -y, -z, -w ); }

	/// \brief		operator adding two vectors
	/// \param		other	the vector to be added to the current one
	/// \returns	a new vector with the sum of the given ones
	inline TVec4 operator+ (const TVec4 &other) const
	{ return TVec4(x + other.x, y + other.y, z + other.z, w + other.w); }

	/// \brief		operator subtracting two vectors
	/// \param		other	the vector to be subtracted from the current one
	/// \returns	a vector with the result of the subtraction
	inline TVec4 operator- (const TVec4 &other) const
	{ return TVec4(x - other.x, y - other.y, z - other.z, w - other.w); }

	/// \brief		operator representing a scalar multiplication
	/// \param		s		the scalar value to be multiplied with
	/// \returns	a vector with the result of the scalar multiplication
	inline TVec4 operator* (const T s) const
	{ return TVec4(x * s, y * s, z * s, w * s); }

	/// \brief		operator representing a scalar division
	/// \param		s		the scalar value to be divided with
	/// \returns	a vector with the result of the scalar division
	inline TVec4 operator/ (const T s) const
	{ return TVec4( x / s, y / s, z / s, w / s); }

	/// \brief		operator adding a vector to the current one
	/// \param		other	the vector to be added
	/// \returns	a reference to the current vector
	inline TVec4& operator+= (const TVec4 &other)
	{ x += other.x; y += other.y; z += other.z; w += other.w; return *this; }

	/// \brief		operator subtracting a vector from the current one
	/// \param		other	the vector to be subtracted
	/// \returns	a reference to the current vector
	inline TVec4& operator-= (const TVec4 &other)
	{ x -= other.x; y -= other.y; z -= other.z; w -= other.w; return *this; }

	/// \brief		operator performing a scalar multiplication
	/// \param		s		the scalar value to be multiplied with
	/// \returns	a reference to the current vector
	inline TVec4& operator*= (const T s)
	{ x *= s; y *= s; z *= s; w *= s; return *this; }

	/// \brief		operator performing a scalar division
	/// \param		s		the scalar value to be divided with
	/// \returns	a reference to the current vector
	inline TVec4& operator/= (const T s)
	{ x /= s; y /= s; z /= s; w /= s; return *this; }

	/// \brief		operator checking for element wise equality of two vectors
	/// \param		other	the vector to be checked against
	/// \returns	true if the given vector is (element wise) equal to the current one, false otherwise
	inline bool operator== (const TVec4 &other) const
	{ return (x == other.x && y == other.y && z == other.z && w == other.w); }

	/// \brief		operator checking for element wise inequality of two vectors
	/// \param		other	the vector to be checked against
	/// \returns	false if the given vector is (element wise) equal to the current one, true otherwise
	inline bool operator!= (const TVec4 &other) const
	{ return (x != other.x || y != other.y || z != other.z || w != other.w); }

	//=================================================================================================================
	// get functions
	//=================================================================================================================

	/// \brief		get function for the first 2 elements of the vector
	/// \returns	a 2D vector with the first 2 elements of the current vector
	inline TVec2<T> xy () const
	{ return TVec2<T>(x, y); }

	/// \brief		get function for the first 3 elements of the vector
	/// \returns	a 3D vector with the first 3 elements of the current vector
	inline TVec3<T> xyz () const
	{ return TVec3<T>(x, y, z); }

	//=================================================================================================================
	// arithmetic functions
	//=================================================================================================================

	/// \brief		calculates the length of the current vector
	/// \returns	the length of the current vector
	inline T length () const
	{ return std::sqrt(lengthSquared()); }

	/// \brief		calculates the squared length of the current vector
	/// \returns	the squared length of the current vector
	inline T lengthSquared () const
	{ return x * x + y * y + z * z + w * w; }

	/// \brief		calculates the sum of all elements of the current vector
	/// \returns	the sum of components
	inline T componentSum () const
	{ return x + y + z + w; }

	/// \brief		calculates the product of all elements of the current vector
	/// \returns	the product of components
	inline T componentProd () const
	{ return x * y * z * w; }

	//=================================================================================================================
	// other functions
	//=================================================================================================================

	/// \brief		checks if all elements of the current vector are equal to zero
	/// \returns	true if all elements are equal to zero, false otherwise
	inline bool isZero () const
	{ return (T)0 == x && (T)0 == y && (T)0 == z && (T)0 == w; }

	/// \brief		determines the minimum element of the current vector
	/// \returns	the minimum component
	inline T minComponent () const
	{ return std::min(std::min(x, y), std::min(z, w)); }

	/// \brief		determines the maximum element of the current vector
	/// \returns	the maximum component
	inline T maxComponent () const
	{ return std::max(std::max(x, y), std::max(z, w)); }

	/// \brief		generates a string representation of the current vector
	/// \returns	a string representing the current vector
	inline std::string toString () const
	{ std::ostringstream oss; oss << "[" << x << ", " << y << ", " << z << ", " << w << "]"; return oss.str(); }

public:
	union
	{
		T		v[4];				///< actual data of the vector
		struct	{ T x, y, z, w; };	///< alternate representation of the vector contents
	};
};

/// \brief		operator multiplying two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the product of the two vectors
template <typename T>
inline TVec4<T> operator* (const TVec4<T> &v1, const TVec4<T> &v2)
{ return TVec4<T>(v1.x * v2.x, v1.y * v2.y, v1.z * v2.z, v1.w * v2.w); }

/// \brief		operator multiplying a scalar and a vector
/// \param		s		left hand side scalar
/// \param		vec		right hand side vector
/// \returns	the product of the scalar and the vector
template <typename T>
inline TVec4<T> operator* (T s, const TVec4<T> &vec)
{ return vec * s; }

/// \brief		operator dividing one vector with another (element wise)
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	a new vector initialized with the element wise division results
template <typename T>
inline TVec4<T> operator/ (const TVec4<T> &v1, const TVec4<T> &v2)
{ return TVec4<T>(v1.x / v2.x, v1.y / v2.y, v1.z / v2.z, v1.w / v2.w); }

/// \brief		operator printing the contents of a vector to an output stream
/// \param		os		the output stream to be written to
/// \param		vec		the vector to be printed
/// \returns	the given output stream reference
template <typename T>
inline std::ostream& operator<< (std::ostream &os, const TVec4<T> &vec)
{ os << "[ " << vec.x << ", " << vec.y << ", " << vec.z << ", " << vec.w << " ]"; return os; }

//=================================================================================================================
// utility functions
//=================================================================================================================

/// \brief		normalizes the given vector
/// \param		vec		the vector to be normalized
/// \returns	a normalized copy of the given vector
/// \note		in case of a vector length of 0, a copy of the original vector is returned
template <typename T>
inline TVec4<T> normalize (const TVec4<T> &vec)
{ T l = vec.length(); return (((T)0 == l) ? vec : vec / l); }

/// \brief		calculates the dot product of two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the dot product of the given vectors
template <typename T>
inline T dot (const TVec4<T> &v1, const TVec4<T> &v2)
{ return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w; }

/// \brief		calculates the absolute value of the dot product of two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the absolute value of the dot product of the given vectors
template <typename T>
inline T absDot (const TVec4<T> &v1, const TVec4<T> &v2)
{ return std::abs(dot(v1, v2)); }

/// \brief		calculates the cross product of two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	the cross product of the given vectors
template <typename T>
inline TVec4<T> cross (const TVec4<T> &v1, const TVec4<T> &v2)
{ return TVec4<T>(v1.y * v2.z - v1.z * v2.y, v1.z * v2.x - v1.x * v2.z, v1.x * v2.y - v1.y * v2.x, 0.f); }

/// \brief		determines the element wise minimum between two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	a vector initialized with the element wise minimum values of the given vectors
template <typename T>
inline TVec4<T> min (const TVec4<T> &v1, const TVec4<T> &v2)
{ return TVec4<T>(std::min(v1.x, v2.x), std::min(v1.y, v2.y), std::min(v1.z, v2.z), std::min(v1.w, v2.w)); }

/// \brief		determines the element wise maximum between two vectors
/// \param		v1		left hand side vector
/// \param		v2		right hand side vector
/// \returns	a vector initialized with the element wise maximum values of the given vectors
template <typename T>
inline TVec4<T> max (const TVec4<T> &v1, const TVec4<T> &v2)
{ return TVec4<T>(std::max(v1.x, v2.x), std::max(v1.y, v2.y), std::max(v1.z, v2.z), std::max(v1.w, v2.w)); }
""");
  }
}
