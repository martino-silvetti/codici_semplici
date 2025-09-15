#include <iostream>
#include <omp.h>
using namespace std;
//to be compiled with -fopenmp

int main(){
	
	#pragma omp parallel
	{
		int ThreadIndex = omp_get_thread_num();
		int NThreads = omp_get_num_threads();
		
		cout << "Ciao da " << ThreadIndex << " su " << NThreads << "\n" << endl;
		
	}
	return 0;
}
