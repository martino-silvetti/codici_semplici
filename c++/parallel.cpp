#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <omp.h>

/*To be compiled with g++ -fopenmp hello_file.cpp -o hello_file*/

int main() {
    std::vector<std::string> messages;
    std::string line;

    // Leggi messaggi da file
    std::ifstream infile("messaggi.txt");
    if (!infile) {
        std::cerr << "Errore nell'apertura di messaggi.txt" << std::endl;
        return 1;
    }

    while (std::getline(infile, line)) {
        messages.push_back(line);
    }
    infile.close();

    // Esecuzione parallela
    #pragma omp parallel
    {
        int id = omp_get_thread_num();
        if (id < messages.size()) {
            std::cout << messages[id] << std::endl;
        } else {
            std::cout << "Thread " << id << " non ha un messaggio disponibile." << std::endl;
        }
    }

    return 0;
}

