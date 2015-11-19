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
//      ______            _____ _                  _ _
//      |  ____|          / ____| |                (_) |
//      | |__  __  ____ _| (___ | |_ ___ _ __   ___ _| |___
//      |  __| \ \/ / _` |\___ \| __/ _ \ '_ \ / __| | / __|
//      | |____ >  < (_| |____) | ||  __/ | | | (__| | \__ \
//      |______/_/\_\__,_|_____/ \__\___|_| |_|\___|_|_|___/
//
/// \file Matrix.h
/// \brief  Header file for Matrix classes
/// \author Christian Schmitt


#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>
#include <tuple>
#include <time.h>

template<typename T, size_t M, size_t N>
class Matrix {
public:
    std::vector< T > m_data;

    
    // default constructor
    Matrix()
    : m_data ( M * N )
    { }

    // constructor
    Matrix ( T value )
    : m_data ( M* N, value )
    { }

    Matrix ( std::vector<T> data ){
        m_data.resize ( M * N );
        std::copy(data.begin(), data.end(), m_data.begin());
    }

    // copy constructor
    Matrix ( const Matrix<T, M, N>& other ) {
        m_data.resize ( M * N );
        std::copy(other.m_data.begin(), other.m_data.end(), m_data.begin());
    }

    Matrix<T, M, N>& operator= ( Matrix<T, M, N> other ) { // pass 'other' by value for implicit copy
        other.swap ( *this );
        return *this;
    }

#ifdef HAS_MOVE_SEMANTICS
    // C++14 move constructor
    Matrix ( Matrix&& other ) noexcept
    :
    m_data ( std::move ( other.m_data ) )
    { }

    // C++14 move assignment operator
    Matrix& operator= ( Matrix&& other ) {
        m_data = std::move ( other.m_data );
        return *this;
    }
#endif

    // destructor
    ~Matrix() { }

    size_t rows() const {
        return M;
    }

    size_t columns() const {
        return N;
    }

    size_t cols() const {
        return this->columns;
    }

    void swap ( Matrix<T, M, N>& other ) noexcept {
        std::swap ( this->m_data, other.m_data );
    }

    void set ( const T& value ) {
        for ( size_t i = 0; i < this->rows(); ++i ) {
            for ( size_t j = 0; j < this->columns(); ++j ) {
                ( *this ) ( i, j ) = value;
            }
        }
    }

    void setRandom ( const T& scale ) {
        srand ( time ( NULL ) );
        for ( size_t i = 0; i < this->rows(); ++i ) {
            for ( size_t j = 0; j < this->columns(); ++j ) {
                ( *this ) ( i, j ) = rand() / scale;
            }
        }
    }

    void setIdentity() {
        for ( size_t i = 0; i < this->rows(); ++i ) {
            for ( size_t j = 0; j < this->columns(); ++j ) {
                if ( i == j ) {
                    ( *this ) ( i, j ) = 1;
                } else {
                    ( *this ) ( i, j ) = 0;
                }
            }
        }
    }

    Matrix<T, 1, N> getRow ( const size_t row ) {
        Matrix<T, 1, N> out;
        for ( size_t i = 0; i < this->columns(); ++i ) {
            out ( 0, i ) = ( *this ) ( row, i );
        }
    }

    Matrix<T, 1, N> getColumn ( const size_t column ) {
        Matrix<T, M, 1> out;
        for ( size_t i = 0; i < this->rows(); ++i ) {
            out ( i, 0 ) = ( *this ) ( i, column );
        }
    }

    void setRow ( const size_t row, const std::vector<T>& values ) {
        for ( size_t i = 0; i < this->columns(); ++i ) {
            ( *this ) ( row, i ) = values ( i );
        }
    }

    void setColumn ( const size_t column, const std::vector<T>& values ) {
        for ( size_t i = 0; i < this->rows(); ++i ) {
            ( *this ) ( i, column ) = values ( i );
        }
    }

    std::tuple< Matrix<T, M, M>, Matrix<T, M, M> > lu() const {
        static_assert ( M == N, "lu() is only defined for square matrices!" );

        Matrix<T, M, M> L, U;
// FIXME add pivoting
        for ( size_t j = 0; j < N; ++j ) {
            for ( size_t i = 0; i < N; ++i ) {
                if ( i <= j ) {
                    U ( i, j ) = ( *this ) ( i, j );
                    if ( i > 0 ) {
                        for ( int k = 0; k <= i - 1; ++k ) {
                            U ( i, j ) -= L ( i, k ) * U ( k, j );
                        }
                    }
                    if ( i == j ) {
                        L ( i, j ) = 1;
                    }
                } else {
                    L ( i, j ) = ( *this ) ( i, j );
                    if ( j > 0 ) {
                        for ( int k = 0; k <= j - 1; ++k ) {
                            L ( i, j ) -= L ( i, k ) * U ( k, j );
                        }
                    }
                    L ( i, j ) /= U ( j, j );
                }
            }
        }

        return std::tuple< Matrix<T, M, M>, Matrix<T, M, M> > ( L, U );
    }


    T determinant() const {
        static_assert ( M == N, "determinant() is only defined for square matrices!" );

        // Compiler should optimize the switch away
        switch ( M ) {
        case 1:
            return ( *this ) ( 0, 0 );
        case 2:
            return ( *this ) ( 0, 0 ) * ( *this ) ( 1, 1 ) - ( *this ) ( 1, 0 ) * ( *this ) ( 0, 1 );
        case 3:
            return ( ( *this ) ( 0, 0 ) * ( *this ) ( 1, 1 ) * ( *this ) ( 2, 2 )
            + ( *this ) ( 0, 1 ) * ( *this ) ( 1, 2 ) * ( *this ) ( 2, 0 )
            + ( *this ) ( 0, 2 ) * ( *this ) ( 1, 0 ) * ( *this ) ( 2, 1 )
            - ( *this ) ( 0, 2 ) * ( *this ) ( 1, 1 ) * ( *this ) ( 2, 0 )
            - ( *this ) ( 0, 1 ) * ( *this ) ( 1, 0 ) * ( *this ) ( 2, 2 )
            - ( *this ) ( 0, 0 ) * ( *this ) ( 1, 2 ) * ( *this ) ( 2, 1 ) );
        default: {
            auto U = std::get<1> ( this->lu() );
            std::cout << U << std::endl;
            T prod = U ( 0, 0 );
            for ( size_t i = 1; i < N; ++i ) {
                prod *= U ( i, i );
            }
            return prod;
        }
        }
    }

    Matrix<T, M, N> inverse() const {
        static_assert ( M == N, "inverse() is only defined for square matrices!" );

        // Compiler should optimize the switch away
        switch ( M ) {
        case 1: {
            Matrix<T, M, N> m ( 1 / ( *this ) ( 0,0 ) );
            return m;
        }
        case 2: {
            Matrix<T, M, N> m;
            m ( 0, 0 ) = ( *this ) ( 1, 1 );
            m ( 1, 0 ) = ( *this ) ( 1, 0 ) * ( -1 );
            m ( 0, 1 ) = ( *this ) ( 0, 1 ) * ( -1 );
            m ( 1, 1 ) = ( *this ) ( 0, 0 );
            return m;
        }
        case 3: {
            Matrix<T, M, N> m;
            const auto det = this->determinant();
            m ( 0, 0 ) = ( ( *this ) ( 1, 1 ) * ( *this ) ( 2, 2 ) - ( *this ) ( 1, 2 ) * ( *this ) ( 2, 1 ) ) / det;
            m ( 0, 1 ) = ( ( *this ) ( 0, 2 ) * ( *this ) ( 2, 1 ) - ( *this ) ( 0, 1 ) * ( *this ) ( 2, 2 ) ) / det;
            m ( 0, 2 ) = ( ( *this ) ( 0, 1 ) * ( *this ) ( 1, 2 ) - ( *this ) ( 0, 2 ) * ( *this ) ( 1, 1 ) ) / det;
            m ( 1, 0 ) = ( ( *this ) ( 1, 2 ) * ( *this ) ( 2, 0 ) - ( *this ) ( 1, 0 ) * ( *this ) ( 2, 2 ) ) / det;
            m ( 1, 1 ) = ( ( *this ) ( 0, 0 ) * ( *this ) ( 2, 2 ) - ( *this ) ( 0, 2 ) * ( *this ) ( 2, 0 ) ) / det;
            m ( 1, 2 ) = ( ( *this ) ( 0, 2 ) * ( *this ) ( 1, 0 ) - ( *this ) ( 0, 0 ) * ( *this ) ( 1, 2 ) ) / det;
            m ( 2, 0 ) = ( ( *this ) ( 1, 0 ) * ( *this ) ( 2, 1 ) - ( *this ) ( 1, 1 ) * ( *this ) ( 2, 0 ) ) / det;
            m ( 2, 1 ) = ( ( *this ) ( 0, 1 ) * ( *this ) ( 2, 0 ) - ( *this ) ( 0, 0 ) * ( *this ) ( 2, 1 ) ) / det;
            m ( 2, 2 ) = ( ( *this ) ( 0, 0 ) * ( *this ) ( 1, 1 ) - ( *this ) ( 0, 1 ) * ( *this ) ( 1, 0 ) ) / det;
            return m;
        }
        default: {
            auto lu = this->lu();
            auto L = std::get<0> ( lu );
            auto U = std::get<1> ( lu );

            Matrix<T, M, N> y;
            Matrix<T, M, N> z;

            for ( size_t j = 0; j < N; ++j ) {
                for ( size_t i = 0; i < M; ++i ) {
                    T sum = 0;
                    for ( size_t k = 0; k < i; ++k ) {
                        sum += L ( i, k ) * y ( k, j );
                    }
                    if ( i == j ) {
                        y ( i, j ) = ( 1 - sum ) / L ( i, i );
                    } else {
                        y ( i, j ) = ( 0 - sum ) / L ( i, i );
                    }
                }

                for ( size_t i = N - 1; i <= SIZE_MAX - 1; --i ) {
                    T sum = 0;
                    for ( size_t k = N - 1; k > i; --k ) {
                        sum += U ( i, k ) * z ( k, j );
                    }
                    z ( i, j ) = ( y ( i, j ) - sum ) / U ( i, i );
                }
            }

            return z;
        }
        }
    }

    Matrix<T, N, M> transpose() const {
        Matrix<T, N, M> out;
        for ( size_t i = 0; i < N; i++ ) {
            for (  size_t j = 0; j < M; j++ ) {
                out( j, i ) = ( *this )( i, j );
            }
        }
        return out;
    }

    T& operator() ( const size_t i, const size_t j ) {
        assert ( i < this->rows() && j < this->columns() );
        return m_data[i * this->columns() + j];
    }

    const T& operator() ( const size_t i, const size_t j ) const {
        assert ( i < this->rows() && j < this->columns() );
        return m_data[i * this->columns() +j];
    }

    T& component ( const size_t i ) {
        #if M == 1
            return ( *this )( 1, i );
        #elif  N == 1
            return ( *this )( i, 1 );
        #else
            return m_data[0]; // FIXME
        #endif
    }

    const T& component ( const size_t i ) const {
        #if M == 1
            return ( *this )( 1, i );
        #elif  N == 1
            return ( *this )( i, 1 );
        #else
            return m_data[0]; // FIXME
        #endif
    }
};

////////////////////////////////////////////////////////////////////////////////
////// Addition operators
////////////////////////////////////////////////////////////////////////////////

template<typename T, size_t M, size_t N>
Matrix<T, M, N>& operator+= ( Matrix<T, M, N>& a, const Matrix<T, M, N>& b ) {
    assert ( a.rows() == b.rows() && a.columns() == b.columns() );
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            a ( i, j ) += b ( i, j );
        }
    }
    return a;
}

template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator+ ( Matrix<T, M, N> a, const Matrix<T, M, N>& b ) { // pass 'a' by value for implicit copy
    a += b;
    return a;
}

////////////////////////////////////////////////////////////////////////////////
////// Subtraction operators
////////////////////////////////////////////////////////////////////////////////

template<typename T, size_t M, size_t N>
Matrix<T, M, N>& operator-= ( Matrix<T, M, N>& a, const Matrix<T, M, N>& b ) {
    assert ( a.rows() == b.rows() && a.columns() == b.columns() );
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            a ( i, j ) -= b ( i, j );
        }
    }
    return a;
}

template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator- ( Matrix<T, M, N> a, const Matrix<T, M, N>& b ) { // pass 'a' by value for implicit copy
    a -= b;
    return a;
}


////////////////////////////////////////////////////////////////////////////////
////// Multiplication operators
////////////////////////////////////////////////////////////////////////////////

// Matrix * Matrix
template<typename T, size_t M, size_t N, size_t A>
Matrix<T, M, N> operator* ( const Matrix<T, M, A>& a, const Matrix<T, A, N>& b ) {
    assert ( a.columns() == b.rows() );
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < b.columns(); ++j ) {
            T t ( 0 );
            for ( size_t k = 0; k < a.columns(); ++k ) {
                t += a ( i, k ) * b ( k, j );
            }
            out ( i, j ) = t;
        }
    }
    return out;
}

// Matrix * Scalar
template<typename T, size_t M, size_t N, size_t A>
Matrix<T, M, N> operator*= ( Matrix<T, M, A>& a, const T& b ) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            out ( i, j ) = a ( i, j ) * b;
        }
    }
    return out;
}
/*
// Matrix * Scalar
template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator* ( const Matrix<T, M, N> a, const T& b ) { // pass 'a' by value for implicit copy
    a *= b;
    return a;
}

// Scalar * Matrix
template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator* ( const T& b, Matrix<T, M, N> a ) { // pass 'a' by value for implicit copy
    a *= b;
    return a;
}
*/
// Matrix * Scalar
template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator* ( const Matrix<T, M, N>& a, const int b ) {
    Matrix<T, M, N> out(a);
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            out ( i, j ) *= b;
        }
    }
    return out;
}

// Scalar * Matrix
template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator* ( const int b, Matrix<T, M, N>& a ) {
    Matrix<T, M, N> out(a);
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            out ( i, j ) = b;
        }
    }
    return out;
}

// Matrix * Scalar
template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator* ( const Matrix<T, M, N>& a, const double b ) {
    Matrix<T, M, N> out(a);
    for ( size_t i = 0; i < out.rows(); ++i ) {
        for ( size_t j = 0; j < out.columns(); ++j ) {
            out ( i, j ) *= b;
        }
    }
    return out;
}

// Scalar * Matrix
template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator* ( const double b, const Matrix<T, M, N>& a ) {
    Matrix<T, M, N> out(a);
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            out ( i, j ) *= b;
        }
    }
    return out;
}




////////////////////////////////////////////////////////////////////////////////
////// Divison operators
////////////////////////////////////////////////////////////////////////////////

// Matrix / Scalar
template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator/= ( Matrix<T, M, N>& a, const T& b ) {
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            a ( i, j ) /= b;
        }
    }
    return a;
}

// Matrix / Scalar
template<typename T, size_t M, size_t N>
Matrix<T, M, N> operator/ ( Matrix<T, M, N> a, const T& b ) { // pass 'a' by value for implicit copy
    a /= b;
    return a;
}


////////////////////////////////////////////////////////////////////////////////
////// Element-wise functions
////////////////////////////////////////////////////////////////////////////////

// Matrix + scalar element-wise
/*
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseAdd(const Matrix<T, M, N>& a, const T& b) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) + b;
        }
    }
    return out;
}

template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseAdd(const T& b, const Matrix<T, M, N>& a) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) + b;
        }
    }
    return out;
}
*/
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseAdd(const Matrix<T, M, N>& a, const double b) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) + b;
        }
    }
    return out;
}
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseAdd(const double b, const Matrix<T, M, N>& a) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) + b;
        }
    }
    return out;
}
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseAdd(const Matrix<T, M, N>& a, const int b) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) + b;
        }
    }
    return out;
}
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseAdd(const int b, const Matrix<T, M, N>& a) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) + b;
        }
    }
    return out;
}

// Matrix - scalar element-wise
/*
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseSub(const Matrix<T, M, N>& a, const T& b) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) - b;
        }
    }
    return out;
}
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseSub(const T& b, const Matrix<T, M, N>& a) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) - b;
        }
    }
    return out;
}
*/
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseSub(const Matrix<T, M, N>& a, const double b) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) - b;
        }
    }
    return out;
}
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseSub(const double b, const Matrix<T, M, N>& a) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) - b;
        }
    }
    return out;
}
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseSub(const Matrix<T, M, N>& a, const int b) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) - b;
        }
    }
    return out;
}
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseSub(const int b, const Matrix<T, M, N>& a) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) - b;
        }
    }
    return out;
}



// Matrix * Matrix element-wise
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseMul(const Matrix<T, M, N>& a, const Matrix<T, M, N>& b) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) * b ( i, j );
        }
    }
    return out;
}

// Matrix / Matrix element-wise
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseDiv(const Matrix<T, M, N>& a, const Matrix<T, M, N>& b) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) / b ( i, j );
        }
    }
    return out;
}

// Matrix % Matrix element-wise
template<typename T, size_t M, size_t N>
Matrix<T, M, N> elementwiseMod(const Matrix<T, M, N>& a, const Matrix<T, M, N>& b) {
    Matrix<T, M, N> out;
    for ( size_t i = 0; i < M; ++i) {
        for ( size_t j = 0; j < N; ++j) {
            out ( i, j ) = a ( i, j ) % b ( i, j );
        }
    }
    return out;
}


// #####################################################################################

template<typename T, size_t M, size_t N>
std::ostream& operator<< ( std::ostream &os, const Matrix<T, M, N>& other ) {
/*    os << "{\n";
    for ( int i = 0; i < other.rows(); i++ ) {
        os << "{ ";
        os << other ( i, 0);
        for ( int j = 1; j < other.columns(); j++ ) {
            os << ", " << other ( i, j );
        }
        os << " }\n";
    }
    os << "} ";
*/
for(int i = 0; i < other.m_data.size(); i++) os  << other.m_data[i] << " ";
    return os;
}

// operators with differing datatypes

template<size_t M, size_t N>
Matrix<double, M, N>& operator+ ( Matrix<double, M, N>& a, const Matrix<int, M, N>& b ) {
    assert ( a.rows() == b.rows() && a.columns == b.columns() );
    Matrix<double, M, N> out;
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            out ( i, j ) = a( i, j ) + b ( i, j );
        }
    }
    return a;
}

template<size_t M, size_t N>
Matrix<double, M, N>& operator+ ( Matrix<int, M, N>& a, const Matrix<double, M, N>& b ) {
    assert ( a.rows() == b.rows() && a.columns == b.columns() );
    Matrix<double, M, N> out;
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            out ( i, j ) = a( i, j ) + b ( i, j );
        }
    }
    return a;
}

template<size_t M, size_t N>
Matrix<double, M, N>& operator- ( Matrix<double, M, N>& a, const Matrix<int, M, N>& b ) {
    assert ( a.rows() == b.rows() && a.columns == b.columns() );
    Matrix<double, M, N> out;
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            out ( i, j ) = a( i, j ) - b ( i, j );
        }
    }
    return a;
}

template<size_t M, size_t N>
Matrix<double, M, N>& operator- ( Matrix<int, M, N>& a, const Matrix<double, M, N>& b ) {
    assert ( a.rows() == b.rows() && a.columns == b.columns() );
    Matrix<double, M, N> out;
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < a.columns(); ++j ) {
            out ( i, j ) = a( i, j ) - b ( i, j );
        }
    }
    return a;
}

template<size_t M, size_t N, size_t A>
Matrix<double, M, N> operator* ( const Matrix<int, M, A>& a, const Matrix<double, A, N>& b ) {
    assert ( a.columns() == b.rows() );
    Matrix<double, M, N> out;
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < b.columns(); ++j ) {
            double t ( 0 );
            for ( size_t k = 0; k < a.columns(); ++k ) {
                t += a ( i, k ) * b ( k, j );
            }
            out ( i, j ) = t;
        }
    }
    return out;
}

template<size_t M, size_t N, size_t A>
Matrix<double, M, N> operator* ( const Matrix<double, M, A>& a, const Matrix<int, A, N>& b ) {
    assert ( a.columns() == b.rows() );
    Matrix<double, M, N> out;
    for ( size_t i = 0; i < a.rows(); ++i ) {
        for ( size_t j = 0; j < b.columns(); ++j ) {
            double t ( 0 );
            for ( size_t k = 0; k < a.columns(); ++k ) {
                t += a ( i, k ) * b ( k, j );
            }
            out ( i, j ) = t;
        }
    }
    return out;
}


template<typename T, size_t M>
Matrix<T, M, M> inverse(const Matrix<T, M, M>& o) {
    return o.inverse();
}

template<typename T, size_t M, size_t N>
Matrix<T, N, M> transpose(const Matrix<T, M, N>& o) {
    return o.transpose();
}

template<typename T, size_t N>
T& getComponent(const Matrix<T, 1, N>& o, size_t i) {
    return o.component(i);
}

template<typename T, size_t M>
T& getComponent(Matrix<T, M, 1>& o, const size_t i) {
    return o.component(i);
}

template<typename T, size_t M>
T& getComponent(const Matrix<T, M, 1>& o, const size_t i) {
    return o.component(i);
}

template<typename T, size_t M>
T dotProduct(const Matrix<T, M, 1>& a, const Matrix<T, M, 1>& b) {
    T out;
    for( size_t i = 0; i < a.rows(); i++ ) {
        for(size_t j = 0; j < a.columns(); j++ ) {
            out += a(i, j) * b(i, j);
        }
    }  
    return out;
}

template<typename T, size_t M>
T dotProduct(const Matrix<T, 1, M>& a, const Matrix<T, 1, M>& b) {
    T out;
    for( size_t i = 0; i < a.rows(); i++ ) {
        for(size_t j = 0; j < a.columns(); j++ ) {
            out += a(i, j) * b(i, j);
        }
    }  
    return out;
}

template<typename T, size_t M>
T dotProduct(const Matrix<T, M, 1>& a, const Matrix<T, 1, M>& b) {
    T out;
    for( size_t i = 0; i < a.rows(); i++ ) {
        for(size_t j = 0; j < a.columns(); j++ ) {
            out += a(i, j) * b(i, j);
        }
    }  
    return out;
}

template<typename T, size_t M>
T dotProduct(const Matrix<T, 1, M>& a, const Matrix<T, M, 1>& b) {
    T out;
    for( size_t i = 0; i < a.rows(); i++ ) {
        for(size_t j = 0; j < a.columns(); j++ ) {
            out += a(i, j) * b(i, j);
        }
    }  
    return out;
}

"""
  }
}
