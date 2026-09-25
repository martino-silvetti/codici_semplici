program reduce

#ifdef _MPI
    use mpi
#endif

implicit none

integer, parameter :: dp = kind(0.d0)
integer :: N
integer :: i
real(dp),  allocatable :: A(:)
real(dp) :: sum


#ifdef _MPI 
    integer :: tmpArrayLength
    integer :: ierr, nprocs, rank
    real(dp) :: singleRankSum
#endif

#ifdef _MPI
! MPI section
    call mpi_init(ierr)
    call mpi_comm_size(mpi_comm_world, nprocs, ierr)
    call mpi_comm_rank(mpi_comm_world, rank, ierr)



    if (rank == 0) then
        write(*,*) "Please insert N length of the arrays"
        read(*,*) N
    endif

    ! use broadcast to pass correctly pass N to all ranks before they start allocating 
    call mpi_bcast(N,1,mpi_integer, 0, mpi_comm_world, ierr)

    !initialise for each task the partial sum to 0
    singleRankSum = 0

    ! if task is the last one and N cannot be divided by nprocs, report subarray length
    if (rank == nprocs - 1) then
        tmpArrayLength = N/nprocs + mod(N, nprocs)
    else
        tmpArrayLength = N/nprocs
    endif
    write(*,*) 'length of temporary arrays in rank ', rank , ' is ',   tmpArrayLength
    

    allocate(A(tmpArrayLength))
     
    ! for nprocs 4 should start at 1,5,9,13 and count resp. to 4,8,12,16
    do i = 1, tmpArrayLength 
        A(i) = (i + rank*nprocs) 
        !write(*,*) 'rank ' , rank , 'A is ', A(i) 
        singleRankSum = singleRankSum + A(i)
    enddo
    
    ! sum in rank 1 all the partial sums
    call mpi_reduce(singleRankSum, sum, 1, mpi_double_precision, mpi_sum, 0, mpi_comm_world, ierr)

    
    write(*,*) 'rank ' , rank , 'sum ', singleRankSum
    flush(6)
    ! wait for all task to end calculation 

    call mpi_barrier(mpi_comm_world, ierr)

    if (rank == 0) then 
        write(*,*) 'mpi result is ', sum
        flush(6)
    endif

    deallocate(A)


 
    call mpi_finalize(ierr)



#else
! serial section

    write(*,*) "Please insert N length of the arrays"
    read(*,*) N

    allocate(A(N))

    sum = 0
    do i = 1 , N
        write(*,*) i 
        A(i) = i
        sum = sum + A(i)
    enddo

    deallocate(A)
    write(*,*) 'result serial'
    write(*,*) sum
#endif




end program reduce
