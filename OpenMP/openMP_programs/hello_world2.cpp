#include <stdio.h>
#include <omp.h>
//to be compiled with -fopenmp



int main(){
	int a=5; // a viene definita PRIMA del costrutto parallel, nel costrutto parallelo e’ shared
	#pragma omp parallel num_threads(3)
	{
		int id; // id e’ definita dentro il costrutto e’ PRIVATA di default
		id = omp_get_thread_num();
	printf("thread: %d, a=%d\n", id, a);
	}
}
