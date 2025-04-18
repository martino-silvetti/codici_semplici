#include <iostream>


/*solve with*/

/* u_i^{n+1} = u_i^{n} + \lambda*(u_i+1^{n}-2*u_i^{n}+u_i-1^{n})*/
/*\lambda=\alpha\frac{\Delta t}{\Delta x^2}*/

int** first_derivative_operator(int size){

	/*convoluted but robust way to define a matrix*/
	/*allocate an array of pointers, a pointer to this array*/

	int** first_derivative_matrix = new int*[size]; /*pointer to an array of pointers*/
	
	for (int i = 0 ; i < size ; i++){
		first_derivative_matrix[i] = new int[size]; /*allocate an array of "size" elements*/


		for (int j = 0 ; j < size ; j++){
			first_derivative_matrix[i][j] = 4;
		}
	}

	return first_derivative_matrix;
}


int main(){
	int i,j;
	
	int a = 5;
	int*p = &a;
	std::cout << p << std::endl; /*print the actual RAM address*/
	std::cout << *p << std::endl; /*print the value the pointers is pointing at*/
	std::cout << &a << std::endl; /*print directly the RAM address where a is stored*/

	int** mat = first_derivative_operator(10);
	
	
	std::cout << mat[0][0] << std::endl;
		
	}
