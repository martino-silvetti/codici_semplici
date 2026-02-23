module grids
use constants
use potentials
implicit none

contains

	subroutine createGridRadial(samplingN, rMin, rMax, grid)
		
		integer :: i, samplingN
		real(dp) :: dr , rMin, rMax 
		real(dp) :: grid(:)
		
		dr = (rMax-rMin)/samplingN
		
		do i = 1,samplingN
			grid(i) = rMin + i*dr
		enddo
	
	end subroutine createGridRadial


	subroutine buildLocalExtPot(rGrid, localPot)
		
		real(dp) :: rGrid(:) , localPot(:)
		integer :: i
		
		do i = 1, size(rGrid)
			localPot(i) = Vext(rGrid(i))
		enddo
		
	end subroutine buildLocalExtPot



	subroutine buildKineticTermDiagonal(rGrid, kineticTermDiagonal)

		real(dp) :: rGrid(:), kineticTermDiagonal(:)
		integer :: i 

		do i = 1 , size(rGrid)
			kineticTermDiagonal(i) = -0.5*(-2.) 
		enddo

	end subroutine buildKineticTermDiagonal

	subroutine buildAngularMomentumPot(rGrid, l, angularMomPot)

		real(dp) :: rGrid(:) , angularMomPot(:)
		integer :: i , l 

		do i = 1, size(rGrid)
			angularMomPot(i) = orbitalPot(l,rGrid(i))
		enddo

	end subroutine buildAngularMomentumPot


	!lower diagonal is equal to upper
	subroutine buildKineticTermUpperDiagonal(rGrid, kineticTermUpperDiagonal)

		real(dp) :: rGrid(:) , kineticTermUpperDiagonal(:)
		integer :: i

		do i= 1 ,size(rGrid)-1	
			kineticTermUpperDiagonal(i) = -0.5*1.
		enddo

	end subroutine buildKineticTermUpperDiagonal


	subroutine buildKineticTermLowerDiagonal(rGrid, kineticTermLowerDiagonal)

		real(dp) :: rGrid(:) , kineticTermLowerDiagonal(:)
		integer :: i

		do i= 1 ,size(rGrid)-1	
			kineticTermLowerDiagonal(i) = -0.5*1.
		enddo

	end subroutine buildKineticTermLowerDiagonal




	subroutine printGrid(grid)
		
		real(dp) :: grid(:)
		integer :: i , samplingN
		
		samplingN = size(grid)
		do i = 1 , samplingN
			write(*,*) grid(i)
		enddo
	
	end subroutine printGrid

end module grids
