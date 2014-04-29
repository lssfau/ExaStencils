/*
void exchangeghostlayer(MyArray<double>& data,MyArray<double>* buffer_send, MyArray<double>* buffer_recv) {

				int globalmax = 0;
				MPI_Allreduce( &localmax, &globalmax, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD );
				double* globalaveragefitness = new double[size];
				MPI_Allgather( &averagefit, 1, MPI_DOUBLE, &globalaveragefitness[0], 1, MPI_DOUBLE, MPI_COMM_WORLD );

				MPI_Status* status = new MPI_Status[2*number_sends];
				MPI_Request *request = new MPI_Request[2*number_sends];
if (Pnb[3] >= 0)
MPI_Isend(&(solution_ghost_edge3_send[0].a[0]), solution[lev].x2_, MPI_REAL, Pnb[3], 3, MPI_COMM_WORLD, &request[3]);
} else
	request[3] = MPI_REQUEST_NULL;
//  MPI_Wait(&request_send[n],status);
if (src_rank != rank) {
			MPI_Irecv(&(solution_ghost_edge3_recv[3].a[0]), solution[lev].x2_, MPI_REAL, src_rank, tag+n, MPI_COMM_WORLD,&request[n+number_sends]);
			flag[n] = 1;
			} else {
				request[n+number_sends] = MPI_REQUEST_NULL;
			    flag[3] = 0;
			}
MPI_Waitall(2*number_sends,request,status);
}
*/

/*
void exchangeghostlayer(MyArray<double>& data,MyArray<double>* buffer_send, MyArray<double>* buffer_recv) {


//	Pnb[EAST]

	
	MPI_Status  PsendStatus;
    MPI_Status  PrecvStatus;
	*/
/*
	if (Pcoords[ROW] != Pdims[ROW]-1) {

		cout << rank << " sending to " << Pnb[NORTH] << " receiving from " << Pnb[SOUTH] << " " << endl; 
			
			MPI_Sendrecv(buffer_recv[SOUTH].begin(),
                 data.ncols(),
                 MPIREAL,
                 Pnb[NORTH],
                 1,
                 buffer_send[SOUTH].begin(),
                 data.ncols(),
                 MPIREAL,
                 Pnb[SOUTH],
                 1,
                 COMM_CART,
                 &PrecvStatus	);

	}
*/
/*	//if (Pcoords[ROW] != 0) {

	if (Pdims[ROW] > 1) {

//		cout << rank << " sending to " << Pnb[NORTH] << " receiving from " << Pnb[SOUTH] << " " << endl; 

		// S -> N
		//if (Pnb[SOUTH] > 0) ||  
		MPI_Sendrecv(buffer_send[SOUTH].begin(),
                 data.ncols(),
                 MPIREAL,
                 Pnb[SOUTH],
                 1,
                 buffer_recv[NORTH].begin(),
                 data.ncols(),
                 MPIREAL,
                 Pnb[NORTH],
                 1,
                 COMM_CART,
                 &PrecvStatus	);

       // MPI_Waitall(4, Pshare[ID].PsendRequest, Pshare[ID].PsendStatus);
		

		 
//		cout << rank << " sending to " << Pnb[SOUTH] << " receiving from " << Pnb[NORTH] << " " << endl; 

		//if ((Pnb[NORTH] > 0) || (rank == 0))
        // N -> S
		MPI_Sendrecv(buffer_send[NORTH].begin(),
                 data.ncols(),
                 MPIREAL,
                 Pnb[NORTH],
                 1,
                 buffer_recv[SOUTH].begin(),
                 data.ncols(),
                 MPIREAL,
                 Pnb[SOUTH],
                 1,
                 COMM_CART,
                 &PrecvStatus	);
	}

    if (Pdims[COL] > 1) {

//		cout << rank << " sending to " << Pnb[WEST] << " receiving from " << Pnb[EAST] << " " << endl; 

		// S -> N
//		if (Pnb[WEST] > 0)
		MPI_Sendrecv(buffer_send[WEST].begin(),
                 data.nrows(),
                 MPIREAL,
                 Pnb[WEST],
                 1,
                 buffer_recv[EAST].begin(),
                 data.nrows(),
                 MPIREAL,
                 Pnb[EAST],
                 1,
                 COMM_CART,
                 &PrecvStatus	);

//       cout << rank << " sending to " << Pnb[EAST] << " receiving from " << Pnb[WEST] << " " << endl; 

//		if (Pnb[EAST] > 0)
        // N -> S
		MPI_Sendrecv(buffer_send[EAST].begin(),
                 data.nrows(),
                 MPIREAL,
                 Pnb[EAST],
                 1,
                 buffer_recv[WEST].begin(),
                 data.nrows(),
                 MPIREAL,
                 Pnb[WEST],
                 1,
                 COMM_CART,
                 &PrecvStatus	);
	}
	MPI_Barrier(COMM_CART);


	//}
	*/
/*
	   MPI_Irecv(Pshare[ID].Pghost[GHOSTSEND][COLL_R][lev].begin(),
              anz,
              MPIREAL,
              VON,
              1,
              COMM_CART,
              &Pshare[ID].PrecvRequest[COLL_R]	);


    // Jetzt steht in data das zu sendende drin
    // Jetzt normal mit ISEND verschicken
    MPI_Isend(Pshare[ID].Pghost[GHOSTRECV][COLL_R][lev].begin(),
              anz,
              MPIREAL,
              NACH,
              1,
              COMM_CART,
              &Pshare[ID].PsendRequest[COLL_R]	);
			  */
//}
