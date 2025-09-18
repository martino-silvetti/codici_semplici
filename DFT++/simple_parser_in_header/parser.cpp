#include "parser.h"
//#include <string>
#include <fstream>
#include <stdio.h>

//implementation of parser.h
void ReadInputFile(const char* FileName ){
	
	printf("\n");
	printf("Reading..\n");
	
	
	//define the file
	std::ifstream file(FileName) ;
	
	//Simple Error manager
	if (!file) {
		printf("Error opening file\n") ;
	}
	else{
		printf("tutto ok\n");
		} 

	//Read the file
	std::string Line ;
	int idx_line = 0 ;
	while (std::getline(file , Line)){
		idx_line++ ;
		printf("Linea %d : %s\n", idx_line , Line.c_str());
	}

}
