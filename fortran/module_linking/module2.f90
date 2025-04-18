module module2
implicit none 
contains

	subroutine subroutine_in_module2(a)
		implicit none
		real(8) :: a
		
		write(*,*) "hello world from module 2"

	end subroutine subroutine_in_module2
	
	subroutine dummy_subroutine_in_module2()
		write(*,*) "hello world but from another subroutine"
	end subroutine dummy_subroutine_in_module2

end module module2
