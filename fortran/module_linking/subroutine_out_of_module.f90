! illustrate how to use a subroutine in a separated file
! but not contained in a specific module

subroutine subroutine_isolated(argument)
implicit none 
    real(8):: argument
    write(*,*)
    write(*,*) "----------------------------------------------------------"
    write(*,*) "this is a call to a subroutine not contained in any module"
    write(*,*) "repeat the argument given to this subroutine" , argument  


end subroutine subroutine_isolated
