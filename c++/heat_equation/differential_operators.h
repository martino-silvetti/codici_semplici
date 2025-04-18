#pragma once


/*to do: create the class matrix generator with methods*/
/*allocate*/
/*delete*/
/*print*/
/*fill with first/second derivative operator*/
/**/


int** null_operator(int size){

	/*convoluted but robust way to define a matrix*/
	/*allocate an array of pointers, a pointer to this array*/

	int** null_matrix = new int*[size]; /*pointer to an array of pointers*/
	
	for (int i = 0 ; i < size ; i++){
		null_matrix[i] = new int[size]; /*allocate an array of "size" elements*/
		for (int j = 0 ; j < size ; j++){
			null_matrix[i][j] = 0;
		}
	}

	return null_matrix;	

}

int** first_derivative_operator(int size){
		
	int** first_derivative_matrix = new int*[size];
	
	for (int i = 0 ; i < size ; i++){
		first_derivative_matrix[i] = new int[size];

		for (int j = 0 ; j < size ; j++){

			if (j == i+1) {
				first_derivative_matrix[i][j] = 1;
			}

			else if (j == i-1){
				first_derivative_matrix[i][j] = -1;
			}

			else{
				first_derivative_matrix[i][j] = 0;
			}
		}
	}
	
	return first_derivative_matrix	;
}


/*remember to free memory*/
