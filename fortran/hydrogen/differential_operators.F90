module differentialOperators
use constants
implicit none

! builds the arrays to be used for local potentials, first and second order differential operators 

integer :: samplingN 

contains 

	subroutine heavyDiagonal(samplingN,matrix) ! do not waste memory using this
		
		integer :: samplingN , i , j
		real(dp) :: matrix
			
		allocate(matrix)
		
		do i = 1, samplingN
			do j = 1, samplingN
				
				if (i == j) then
					matrix(i,j) = 1.
				elseif (i .ne. j) then
					matrix(i,j) = 0.
				endif 

			enddo
		enddo

	end subroutine heavyDiagonal



	subroutine smartDiagonal(samplingN , diagonal)
		
		integer :: i

		! allocate diagonal(samplingN) ! allocate outside
		
			do i = 1, samplingN
				diagonal(i) = 1.
			enddo
	
	end subroutine smartDiagonal	
	

	!upper and lower diagonal
	subroutine firstOrderDiffOp(samplingN, ld , ud, semiloca)
		
		integer :: i
		
		! construct only upper and lower diagonal
		do i = 1 ,samplingN - 1 
			ld(i) = -1
			ud(i) = 1
		enddo
	
	
	end subroutine firstOrderDiffOp

	! upper, lower and proper diagonals
	subroutine secondOrderDiffOp(samplingN, ld, d , ud)
	
		integer :: i
		
		! diagonal
		do i = 1, samplingN
			d(i) = -2.
		enddo
		
		! upper and lower diagonals
		do i = 1, samplingN-1
			ld(i) = 1.
			ud(i) = 1.
		enddo 
		
	end subroutine secondOrderDiffOp
	
	
	
	subroutine 
	
end module differentialOperators
