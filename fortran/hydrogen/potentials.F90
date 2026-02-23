module potentials
use constants
implicit none

contains

	real(dp) function Vext(r)

		real(dp) :: r
	
		! external potential (central + e.m. perturbation) in a.u.
		Vext = 1/r

	end function Vext


	real(dp) function orbitalPot(l,r)

		real(dp) :: r
		integer :: l
		
		!angular momentum orbital effective central potential
		orbitalPot = (l*(l+1))/(2*(r*r))

	end function orbitalPot

end module potentials
