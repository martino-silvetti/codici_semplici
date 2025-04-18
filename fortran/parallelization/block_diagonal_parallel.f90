program block_diagonal
implicit none 

integer:: i,j
integer:: A(0:5,0:5)

do i = 0 , 5 
    do j = 0 , 5
        if (i == j .or. i == j+1 .or. i==j-1) then 
            A(i,j) = i+j
        else
            A(i,j) = 0
        end if
    end do
end do

write(*,*) A(0,0)
write(*,*) A

end program block_diagonal
