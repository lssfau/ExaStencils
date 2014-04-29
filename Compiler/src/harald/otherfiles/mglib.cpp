
using namespace std;


#ifndef MATHPI
#define MATHPI    3.14159265358979323846f
#endif

//double sqr(double s) {return s*s;}

template<class T> T* readImageSiemens(const char* fname, int rows, int cols)
{

  std::cout << "readSiemensGroundTruthVolume..." << std::endl;

//  bild.resize ( rows, cols );
  T* convert = new T[rows*cols];

    std::cout << "Open: " << fname << std::endl;

    FILE* fd2 = 0;
    if ( ( fd2 = fopen( fname, "rb" )) == 0 )
    {
      std::cout << "Cannot open file  " << fname << std::endl;
      exit(0);
    }
    fread( &convert[0], sizeof(T), rows*cols, fd2);
	/*
      for ( int i = 0; i < rows; i++ )
      {
        for ( int j = 0; j < cols; j++ )
        {
          bild(i,j) = static_cast<T> (convert[i*cols + j]);
        }
      }
	  */
    fclose( fd2 );

  return convert;
//  delete [] convert;
}


template<class T> void readImage (T& bild, const char *fname )
{

	FILE *input;

	char line[1024];
	short ftype=-1;     // 0 grauwert
                       //                  				1 farbe ascii
                        //                			 	2 farbe bin 

	int nCols,nRows;
	int levels;
	//int byteR, byteG, byteB;

	if ( !fname || ( input=fopen ( fname,"rb" ) ) ==0 )
	{
		std::cerr << "Can't open " << fname << "Aborting! " << std::endl;
		exit ( -1 ); //return -1;
	}

	if ( fread ( line,1,3,input ) !=3 )
	{
		fclose ( input );
		fprintf ( stderr,"Error Wrong Magic field\n" );
	}
	else
	{
		// P2 is ascii!!!
		if	( 0 == strncmp ( line,"P5\n",3 ) ) { ftype = 0; }
		else if ( 0 == strncmp ( line,"P6\n",3 ) ) { ftype = 2; }
		else if ( 0 == strncmp ( line,"P3\n",3 ) ) { ftype = 1; }
		else
		{
			fclose ( input );
			fprintf ( stderr,"Error Wrong magic %s #\n",line );
			exit ( -1 ); //return -1;
		}
	}

	do
		fgets ( line,sizeof line,input );
	while ( *line=='#' );

	sscanf ( line,"%d %d\n",&nCols,&nRows );

	fgets ( line,sizeof line,input );
	sscanf ( line,"%d\n",&levels );
	//int colors = levels;

    T* convert = new T[nRows*nCols];
    if ( ftype == 0 )
	{

//		bild.resize ( nRows, nCols );
		//cerr << "[readImage] resize auf nRows: " << nRows << " nCols: " << nCols << endl;
		// read pixel row by row
		for ( int i1=0;i1<nRows;i1++ )
		{
			for ( int j1=0;j1<nCols;j1++ )
			{
				int byte=fgetc ( input );

				if ( byte==EOF )
				{
					fclose ( input );
					std::cerr << "read failed" << std::endl;
				}
				else
				{
//					convert[i1*nCols + j1] = byte;
					bild ( i1,j1 ).real( byte);
				}
			}
		}
	}

	fclose ( input );
//	return convert;
}


  template<class T> void writeImage(T& bild, string fname)
  {

    FILE* output;
    double rmaxv = 0,rminv = 0;

	output = fopen(fname.c_str(),"wb");
    if (output == NULL)
    {
      cout << "file open failed!" << endl;
      exit(0);
    }

    int byteR = 0,byteG = 0,byteB = 0;
    double byte = 0;

    double maxv=0, minv=0;
      for(int i = 0; i < bild.x1_; i++)
        for(int j = 0; j < bild.x2_; j++) {
			if (bild(i,j) > maxv)
				maxv = bild(i,j);
			if (bild(i,j) < minv)
				minv = bild(i,j);
		}


      rmaxv = 255;
      rminv = 0;

    cout << "Plotting: " << fname << " maxv: " << maxv << " minv: " << minv << endl;

      int colors = 255;

      fprintf(output,"P5\n");

      fprintf(output,"#\n"); /* empty comment */
      fprintf(output,"%d %d\n%03d\n",bild.x1_,bild.x2_,colors); /* image info */

	  double divid = fabs(maxv-minv);

      for(int i = 0; i < bild.x1_; i++)
      {
        for(int j = 0; j < bild.x2_; j++)
        {

            byte= 255.0*(bild(i,j) - minv)/(divid);

            if (byte > 255)
              byte = 255;
            else if (byte < 0)
              byte = 0;

            if (fputc(byte,output)==EOF)
            {
              fclose(output);
            }
          }
    }

    fclose(output);
  }

  template<class T> void writeImage_re(T& bild, string fname)
  {

    FILE* output;
    double rmaxv = 0,rminv = 0;

	output = fopen(fname.c_str(),"wb");
    if (output == NULL)
    {
      cout << "file open failed!" << endl;
      exit(0);
    }

    int byteR = 0,byteG = 0,byteB = 0;
    double byte = 0;

    double maxv=0, minv=0;
      for(int i = 0; i < bild.x1_; i++)
        for(int j = 0; j < bild.x2_; j++) {
			if (bild(i,j).real() > maxv)
				maxv = bild(i,j).real();
			if (bild(i,j).real() < minv)
				minv = bild(i,j).real();
		}


      rmaxv = 255;
      rminv = 0;

    cout << "Plotting: " << fname << " maxv: " << maxv << " minv: " << minv << endl;

      int colors = 255;

      fprintf(output,"P5\n");

      fprintf(output,"#\n"); /* empty comment */
      fprintf(output,"%d %d\n%03d\n",bild.x1_,bild.x2_,colors); /* image info */

	  double divid = fabs(maxv-minv);

      for(int i = 0; i < bild.x1_; i++)
      {
        for(int j = 0; j < bild.x2_; j++)
        {

            byte= 255.0*(bild(i,j).real() - minv)/(divid);

            if (byte > 255)
              byte = 255;
            else if (byte < 0)
              byte = 0;

            if (fputc(byte,output)==EOF)
            {
              fclose(output);
            }
          }
    }

    fclose(output);
  }

    template<class T> void writeImage_im(T& bild, string fname)
  {

    FILE* output;
    double rmaxv = 0,rminv = 0;

	output = fopen(fname.c_str(),"wb");
    if (output == NULL)
    {
      cout << "file open failed!" << endl;
      exit(0);
    }

    int byteR = 0,byteG = 0,byteB = 0;
    double byte = 0;

    double maxv=0, minv=0;
      for(int i = 0; i < bild.x1_; i++)
        for(int j = 0; j < bild.x2_; j++) {
			if (bild(i,j).imag() > maxv)
				maxv = bild(i,j).imag();
			if (bild(i,j).imag() < minv)
				minv = bild(i,j).imag();
		}


      rmaxv = 255;
      rminv = 0;

    cout << "Plotting: " << fname << " maxv: " << maxv << " minv: " << minv << endl;

      int colors = 255;

      fprintf(output,"P5\n");

      fprintf(output,"#\n"); /* empty comment */
      fprintf(output,"%d %d\n%03d\n",bild.x1_,bild.x2_,colors); /* image info */

	  double divid = fabs(maxv-minv);

      for(int i = 0; i < bild.x1_; i++)
      {
        for(int j = 0; j < bild.x2_; j++)
        {

            byte= 255.0*(bild(i,j).imag() - minv)/(divid);

            if (byte > 255)
              byte = 255;
            else if (byte < 0)
              byte = 0;

            if (fputc(byte,output)==EOF)
            {
              fclose(output);
            }
          }
    }

    fclose(output);
  }
