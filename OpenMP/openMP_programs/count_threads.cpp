#include <stdio.h>
#include <iostream>
#include <omp.h>
//to be compiled with -fopenmp
//finds the number of cores, not cpus

int StepMessage(int Status){

	//this function prints on screen what is the current step
	switch (Status){
		
		case 1 :
			ExitStringMessage = "Initialize number of cores"
			break;
		
		case 2:
			ExitStringMessage = "Setting a new number of cores"
			break;
			
		case 3 :
			ExitStringMessage = "Exiting the fork, join"
			break;
	
		} ;
	

	return ExitStringMessage
}

void MessageNumberProcessors(){
	# pragma omp parallel //fork
	{
		int NumberProcessors = omp_get_num_procs() ; 
		std::cout << "Number of processors: " << NumberProcessors << std::endl ;
		
	}

}


void InitMessage(){
	std::cout << "Start Program" << std::endl ; 
}

void StartThreads(int NumberThreads){
	#pragma omp parallel num_threads(NumberThreads) ; 
	{
		int ThreadIdentifier = ompt_get_threadnum();
		
	}
}
	

int main(){
	
	
	return 0 ;
}
