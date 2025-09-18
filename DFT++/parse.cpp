#include <iostream>
#include <stdio.h>
using namespace std ; 

//simple test from parsing

int main(int argc , char* argv[]){
	printf("Numero di argomenti :  %d\n", argc-1) ;

	for (int idx_arg = 1 ; idx_arg < argc ; idx_arg++){

		printf("Argomento %d: %s\n", idx_arg, argv[idx_arg]);

	}

	return 0 ; 
}
