#include <iostream>
#include <string>
#include <omp.h>

int main() {
    const int max_threads = 8;
    std::string messages[max_threads] = {
        "Ciao dal thread 0 - Il pensatore!",
        "Saluti dal thread 1 - Il curioso.",
        "Hey, sono il thread 2 - Il veloce!",
        "Thread 3 qui - Il silenzioso.",
        "Eccomi! Thread 4 - L'ottimista.",
        "Thread 5 - Il logico.",
        "Thread 6 - Il creativo.",
        "Thread 7 - Il misterioso."
    };

    #pragma omp parallel
    {
        int id = omp_get_thread_num();
        if (id < max_threads) {
            std::cout << messages[id] << std::endl;
        } else {
            std::cout << "Thread " << id << " non ha un messaggio assegnato." << std::endl;
        }
    }

    return 0;
}
