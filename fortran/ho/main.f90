program harmonic_oscillator
use algebra_driver , only : eigensolver
use potential_driver ,  only : harmonic_potential
use initialize , only : partition
use io_stream , only : external_reader, external_writer
implicit none

call external_reader(file_name)


end program harmonic_oscillator
