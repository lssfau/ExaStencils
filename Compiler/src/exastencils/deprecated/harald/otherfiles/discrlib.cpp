/*
std::complex<double> getcoeff(MyArray<std::complex<double>>* Sol, int i, int j) {
  
  std::complex<double> coeff;

  const double sigma = 1.0;
  const int kdiff = 2;
  const double sigmakdiffinv = 1.0/(kdiff*sigma*kdiff*sigma);

  double denominv = 1.0/( 1.0 + Sol[lev] ( i,j ).imag()*Sol[lev] ( i,j ).imag() * sigmakdiffinv);
//  coeff[lev] ( i,j ).real( cos ( sigma ) * denominv);
//  coeff[lev] ( i,j ).imag( sin ( sigma ) * denominv);
  coeff.real( cos ( sigma ) * denominv);
  coeff.imag( sin ( sigma ) * denominv);
			
  return coeff;
}

void compst ( MyArray<std::complex<double>>* Sol, int i, int j)
{

	set(1, 0.5* (getcoeff(Sol, i,j-1 ) +getcoeff(Sol, i,j ) ));
	set(2, 0.5* (getcoeff(Sol, i,j+1 ) +getcoeff(Sol, i,j ) ));
	set(3, 0.5* (getcoeff(Sol, i-1,j ) +getcoeff(Sol, i,j ) ));
	set(4, 0.5* (getcoeff(Sol, i+1,j ) +getcoeff(Sol, i,j ) ));

	set(0, (getcoeff(Sol, i,j-1 ) +getcoeff(Sol, i,j+1 ) +getcoeff(Sol, i-1,j ) +getcoeff(Sol, i+1,j ) + 4.0*getcoeff(Sol,  i,j ) ) *fac + 0.01);

}
*/
/*
std::complex<double> getcoeff(int lev, MyArray<std::complex<double>>* Sol, int i, int j) {
  
  std::complex<double> coeff;

  const double sigma = 1.0;
  const int kdiff = 2;
  const double sigmakdiffinv = 1.0/(kdiff*sigma*kdiff*sigma);

  double denominv = 1.0/( 1.0 + Sol[lev] ( i,j ).imag()*Sol[lev] ( i,j ).imag() * sigmakdiffinv);
//  coeff[lev] ( i,j ).real( cos ( sigma ) * denominv);
//  coeff[lev] ( i,j ).imag( sin ( sigma ) * denominv);
  coeff.real( cos ( sigma ) * denominv);
  coeff.imag( sin ( sigma ) * denominv);
			
  return coeff;
}

void compst ( int lev, MyArray<std::complex<double>>* Sol, int i, int j, double fac, MyStencil<std::complex<double>>& st )
{

	st.set(1, 0.5* (getcoeff(lev,Sol, i,j-1 ) +getcoeff(lev,Sol, i,j ) ));
	st.set(2, 0.5* (getcoeff(lev,Sol, i,j+1 ) +getcoeff(lev,Sol, i,j ) ));
	st.set(3, 0.5* (getcoeff(lev,Sol, i-1,j ) +getcoeff(lev,Sol, i,j ) ));
	st.set(4, 0.5* (getcoeff(lev,Sol, i+1,j ) +getcoeff(lev,Sol, i,j ) ));

	st.set(0, (getcoeff(lev,Sol, i,j-1 ) +getcoeff(lev,Sol, i,j+1 ) +getcoeff(lev,Sol, i-1,j ) +getcoeff(lev,Sol, i+1,j ) + 4.0*getcoeff(lev,Sol,  i,j ) ) *fac + 0.01);

}
*/

/*
void compst ( int lev, MyArray<std::complex<double>>* coeff, int i, int j, double fac, MyStencil<std::complex<double>>& st )
{

	st.set(1, 0.5* ( coeff[lev] ( i,j-1 ) + coeff[lev] ( i,j ) ));
	st.set(2, 0.5* ( coeff[lev] ( i,j+1 ) + coeff[lev] ( i,j ) ));
	st.set(3, 0.5* ( coeff[lev] ( i-1,j ) + coeff[lev] ( i,j ) ));
	st.set(4, 0.5* ( coeff[lev] ( i+1,j ) + coeff[lev] ( i,j ) ));

	st.set(0, ( coeff[lev] ( i,j-1 ) + coeff[lev] ( i,j+1 ) + coeff[lev] ( i-1,j ) + coeff[lev] ( i+1,j ) + 4.0*coeff[lev] ( i,j ) ) *fac + 0.01);

}
*/
