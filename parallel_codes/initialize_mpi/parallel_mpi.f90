program parallel_mpi
#ifdef _MPI
    use mpi
#endif
implicit none
integer ::  ierr, nprocs, my_rank

! whatever nprocs it is overwritten by mpi_comm_size
write(*,*) nprocs

! mpi section, compile with  
! compile with mpif90 -cpp -D_MPI parallel_mpi.f90
! run with mpirun -np 3 a.out
# ifdef _MPI
    call mpi_init(ierr)
    call mpi_comm_size(mpi_comm_world, nprocs, ierr )
    call mpi_comm_rank(mpi_comm_world , my_rank, ierr)
    write(*,*) 'process ', my_rank, ' of ' , nprocs, ' processes' 
    call mpi_finalize(ierr)
# endif

! serial section
! compile with gfortran -cpp parallel_mpi.f90
! run with ./a.out
# ifndef _MPI
    write(*,*) 'esecuzione seriale'
#endif


end program parallel_mpi
