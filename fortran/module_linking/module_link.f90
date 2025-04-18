program module_link
!to be compiled and linked in order : 
!gfortran module1.f90 module2.f90 subroutine_out_of_module.f90 module_link.f90 -o test

use module1, only : subroutine_in_module1
use module2, only : subroutine_in_module2

	
implicit none
integer, parameter :: dp = kind(0.d0)
real(dp) :: dummy_argument1, dummy_argument2, dummy_argument3

	dummy_argument1 = 1.0
	dummy_argument2 = 2.0
	dummy_argument3 = 3.0

    ! call the subroutine contained in the modules defined above
	call subroutine_in_module1(dummy_argument1)
	call subroutine_in_module2(dummy_argument2)

    ! call a subroutine compiled separately and not included in any module
    ! no need to declare it in advance
    call subroutine_isolated(dummy_argument3)

end program module_link

