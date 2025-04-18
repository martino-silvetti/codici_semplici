#include <iostream>
#include "differential_operators.h"


/*solve with*/

/* u_i^{n+1} = u_i^{n} + \lambda*(u_i+1^{n}-2*u_i^{n}+u_i-1^{n})*/
/*\lambda=\alpha\frac{\Delta t}{\Delta x^2}*/

int main(){
	int size = 10;
	int** a = first_derivative_operator(size) ;
	for (int i = 0 ; i < size ; i++){
		for (int j = 0 ; j < size ; j++){
			std::cout << a[i][j] << std::endl;
		}
	}
	return 1;
}
