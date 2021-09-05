program name2
    implicit none
    character(100) in, out
    read(*,'(a100)')in
    call abab (in, out)
    write(*,'(a100)')adjustl(out)
end program name2

subroutine abab(arg1,  arg2)
    implicit none
    character(*),intent(in) :: arg1
    character(len(arg1)),intent(out) ::  arg2
    integer :: i, j=1
    do i = len(arg1), 1, -1
        arg2(j:j)=arg1(i:i)
        j = j + 1
    end do
    end subroutine abab