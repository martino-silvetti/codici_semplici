program main
use potentials
use constants
use grids
use hamiltonian
implicit none

integer :: samplingN, l , info
real(dp) :: rMax
real(dp), parameter :: rMin = 0.
real(dp), allocatable :: radialGrid(:), diagHamilt(:), upperHamilt(:) , lowerHamilt(:)
real(dp) :: dummyEigVec(1,1)

external dstev

rMax = 10.
samplingN = 10
! angular momentum quantum number
l = 0



!create radial space grid
allocate(radialGrid(samplingN))
call createGridRadial(samplingN, rMin, rMax, radialGrid)


!create Hamiltonian operator in a.u.
allocate(diagHamilt(samplingN))
allocate(upperHamilt(samplingN-1))
allocate(lowerHamilt(samplingN-1))

call buildHamiltDiagonal(radialGrid, l, diagHamilt)
call buildHamiltUpperDiagonal(radialGrid, upperHamilt)
call buildHamiltLowerDiagonal(radialGrid, lowerHamilt)


write(*,*) "radiaGrid" , radialGrid , " size " , size(radialGrid)
write(*,*) "diagHamilt" , diagHamilt , " size " , size(diagHamilt)
write(*,*) "upperHamilt" , upperHamilt, " size " , size(upperHamilt)
write(*,*) "lowerHamilt" , lowerHamilt, " size " , size(lowerHamilt)
!diagonalize
call dstev('N', samplingN , diagHamilt, upperHamilt , dummyEigVec , 1, info)

deallocate(diagHamilt)
deallocate(upperHamilt)
deallocate(lowerHamilt)
deallocate(radialGrid)


end program main
