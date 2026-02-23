module hamiltonian
use constants
use potentials
use grids
implicit none


!call buildHamiltDiagonal(localExtPot)
!call buildHamiltUpperDiagonal()
!call buildHamiltLowerDiagonal()

contains
	
	subroutine buildHamiltDiagonal(rGrid, l, Hdiagonal)
		
		integer :: l
		real(dp), intent(in) :: rGrid(:) ! allocate in main
		real(dp), allocatable :: nuclearPot(:) , kineticTermDiagonal(:) , angularMomPot(:)
		real(dp) , intent(out) :: Hdiagonal(:) ! allocate in main 
	
		allocate(nuclearPot(size(rGrid)))
		allocate(kineticTermDiagonal(size(rGrid)))
		allocate(angularMomPot(size(rGrid)))

		call buildLocalExtPot(rGrid, nuclearPot)
		call buildKineticTermDiagonal(rGrid, kineticTermDiagonal)
		call buildAngularMomentumPot(rGrid, l, angularMomPot)
		
		Hdiagonal = kineticTermDiagonal + angularMomPot + nuclearPot 
		
	end subroutine buildHamiltDiagonal


	!upper and lower diagonal terms have only kinetic components
	!todo: find a way to add more terms in an easy way 
	subroutine buildHamiltUpperDiagonal(rGrid, upperHamilt)
		
		integer :: i 
		real(dp) :: rGrid(:), upperHamilt(:) ! allocate in main
		real(dp), allocatable :: kineticTermUpperDiagonal(:)

		allocate(kineticTermUpperDiagonal(size(rGrid)-1))
		call buildKineticTermUpperDiagonal(rGrid, kineticTermUpperDiagonal)

		upperHamilt = kineticTermUpperDiagonal 
		
		deallocate(kineticTermUpperDiagonal)
		
	end subroutine buildHamiltUpperDiagonal

	subroutine buildHamiltLowerDiagonal(rGrid, lowerHamilt)
		
		integer :: i 
		real(dp) :: rGrid(:), lowerHamilt(:) ! allocate in main
		real(dp), allocatable :: kineticTermLowerDiagonal(:)

		allocate(kineticTermLowerDiagonal(size(rGrid)-1))
		call buildKineticTermLowerDiagonal(rGrid, kineticTermLowerDiagonal)

		lowerHamilt = kineticTermLowerDiagonal 
		
		deallocate(kineticTermLowerDiagonal)
		
	end subroutine buildHamiltLowerDiagonal


end module hamiltonian

