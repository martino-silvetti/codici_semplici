module module1
implicit none 
contains

	subroutine subroutine_in_module1(a)
		implicit none
		real(8) :: a
		
		write(*,*) "hello world from module 1"

	end subroutine subroutine_in_module1
	
	subroutine dummy_subroutine_in_module1()
		write(*,*) "hello world but from another subroutine"
	end subroutine dummy_subroutine_in_module1

end module module1
