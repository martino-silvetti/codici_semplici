module constants
implicit none
integer, parameter :: dp = kind(0.d0)

real(dp) , parameter :: hbar =  6.582119569E-16 ! eV s 
real(dp) , parameter :: c = 299792458 ! m s-1
real(dp) , parameter :: pi = atan(1.d0)
real(dp) , parameter :: epsilon0SI = 8.8541878188E-12 ! C/V   C^2 eV^-1
real(dp) , parameter :: e2coulomb = 1.602176634E-19 ! C 
real(dp) , parameter :: epsilon0 = epsilon0SI/e2coulomb


end module constants
