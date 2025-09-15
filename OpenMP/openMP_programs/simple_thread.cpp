#include <stdio.h>
#include <iostream>
#include <omp.h>
//to be compiled with -fopenmp


int main(){
	std::cout << "Fork" << std::endl ; 
	
	// find the number of processors
	int NumberProcessors = omp_get_num_procs() ; 
	
	// set a number of threads equal to the number of processors
	omp_set_num_threads(NumberProcessors) ;

	//See if before pragma omp the multithreading is active
	int ParallelStatus = omp_in_parallel() ; 
	std::cout << "Before pragma" <<  std::endl ; 
	if ( ParallelStatus == 0 ){
		std::cout << "Parallelization is not active at this stage" << std::endl ;
	}
	else{
	 std::cout << "Parallelization is not active at this moment" << std::endl ;
	}
	std::cout << "Number of threads currently active" << omp_get_num_threads() << std::endl;
	
	// fork
	#pragma omp parallel  
	{
		
		int NumberThreads = omp_get_num_threads() ;
		
		//printf avoid a race condition
		printf("Number of threads active: %d\n" , NumberThreads) ;
		printf("thread number %d\n" , omp_get_thread_num()) ; 

		//std::cout messes up the output mixing all the threads
		//std::cout << "Number of threads active" << NumberThreads << std::endl ;
		//std::cout << "thread number " << omp_get_thread_num() << std::endl ;
	} 
	
	return 0 ;
}

