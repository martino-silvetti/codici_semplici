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
	std::cout << p[0] << std::endl;

	//int** mat = first_derivative_operator(10);
	
	std::cout << "----------------" << std::endl;
	std::cout << "arrays are created as pointers " << std::endl;
	int*vec = new int[10];
	std::cout << "printing an array gives only the address, i.e. the pointer itself: vec = " << vec << std::endl;	
	std::cout << "printing the particular entry of the pointer dereferenciate it like with the & symbol: vec[0] = " << vec[0] << std::endl;	
	
	
	//std::cout << mat[0][0] << std::endl;
		
	}
