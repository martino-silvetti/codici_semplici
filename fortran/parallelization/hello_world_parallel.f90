program hello_parallel
  use omp_lib
  implicit none
  integer :: ThreadId , NumberOfThreads


NumberOfThreads = omp_get_num_threads()
write(*,*) 'Number of threads declared outside of the parallel section: ',NumberOfThreads
!use differently different threads
!$omp parallel private(ThreadId)

    NumberOfThreads = omp_get_num_threads() !read the OMP_NUM_THREADS global variable declared 
    write(*,*) 'Number of threads declared inside of the parallel section: ',NumberOfThreads
    ThreadId = omp_get_thread_num()
    write(*,*) 'id of the current thread: ',ThreadId
!$omp end parallel


!!$omp parallel private(id)
!  id = omp_get_thread_num()
!  nthreads = omp_get_num_threads()
!  print *, 'Hello, world from thread ', id, ' out of ', nthreads
!!$omp end parallel

end program hello_parallel

