module GYS
    implicit none
    CONTAINS
    SUBROUTINE QG(j,k,y)
        implicit none
        integer:: j,k,y
        do y = min(j, k), 1, -1
            if ( mod(j, y) == 0 .and. mod(k, y) == 0 ) exit
        end do
    end subroutine
    end module GYS
program m
    USE GYS
    implicit none
    integer::j,k,y
    read(*,*)j,k
    call QG(j,k,y)
    write(*,*)y
end program 



    
    